package Types::XSD;

use 5.008003;
use strict;
use warnings;
use utf8;

BEGIN {
	$Types::XSD::AUTHORITY = 'cpan:TOBYINK';
	$Types::XSD::VERSION   = '0.000_01';
}

use B qw(perlstring);
use Carp;
use DateTimeX::Auto qw( dt dur );
use DateTime::Incomplete ();
use XML::RegExp;

our $T;

sub create_range_check
{
	my $class = $_[0]; eval "require $class";
	my ($lower, $upper) = map(defined($_) ? $class->new($_) : $_, @_[1,2]);
	my ($lexcl, $uexcl) = map(!!$_, @_[3,4]);
	
	my $checker =
		(defined $lower and defined $upper and $lexcl and $uexcl)
			? sub { my $n = $class->new($_); $n > $lower and $n < $upper } :
		(defined $lower and defined $upper and $lexcl)
			? sub { my $n = $class->new($_); $n > $lower and $n <= $upper } :
		(defined $lower and defined $upper and $uexcl)
			? sub { my $n = $class->new($_); $n >= $lower and $n < $upper } :
		(defined $lower and defined $upper)
			? sub { my $n = $class->new($_); $n >= $lower and $n <= $upper } :
		(defined $lower and $lexcl)
			? sub { $class->new($_) > $lower } :
		(defined $upper and $uexcl)
			? sub { $class->new($_) < $upper } :
		(defined $lower)
			? sub { $class->new($_) >= $lower } :
		(defined $upper)
			? sub { $class->new($_) <= $upper } :
		sub { !!1 };
	
	my $inlined = sub {
		my $var = $_[1];
		my @checks;
		push @checks, sprintf('$n >%s "%s"->new("%s")', $lexcl?'':'=', $class, $lower) if defined $lower;
		push @checks, sprintf('$n <%s "%s"->new("%s")', $uexcl?'':'=', $class, $upper) if defined $upper;
		my $code = sprintf(
			'%s and do { my $n = "%s"->new(%s); %s }',
			Types::Standard::Int()->inline_check($var),
			$class,
			$var,
			join(" and ", @checks),
		);
	};
	
	return (
		constraint  => $checker,
		inlined     => $inlined,
	);
}

sub quick_range_check
{
	my $class = $_[0]; eval "require $class";
	my ($lower, $upper) = map(defined($_) ? $class->new($_) : $_, @_[1,2]);
	my ($lexcl, $uexcl) = map(!!$_, @_[3,4]);
	my $var = $_[5];
	my @checks;
	push @checks, sprintf('$n >%s "%s"->new("%s")', $lexcl?'':'=', $class, $lower) if defined $lower;
	push @checks, sprintf('$n <%s "%s"->new("%s")', $uexcl?'':'=', $class, $upper) if defined $upper;
	my $code = sprintf(
		'do { my $n = "%s"->new(%s); %s }',
		$class,
		$var,
		join(" and ", @checks),
	);
}

use constant MAGIC_DATES => map dt($_), qw( 1696-09-01 1697-02-01 1903-03-01 1903-07-01 );
use constant MAGIC_TABLE => +{ "-1-1-1-1" => -1, "0000" => 0, "1111" => 1 };
sub dur_cmp
{
	my @durations = map ref($_) ? $_ : dur($_), @_[0,1];
	my $result    = join q[], map "DateTime::Duration"->compare(@durations, $_), MAGIC_DATES;
	return MAGIC_TABLE->{$result} if exists MAGIC_TABLE->{$result};
	return undef;
}

sub hex_length
{
	my $str = shift;
	my $len = ($str =~ tr/0-9A-Fa-f//);
	$len / 2;
}

sub b64_length
{
	my $str = shift;
	$str =~ s/[^a-zA-Z0-9+\x{2f}=]//g;
	my $padding = ($str =~ tr/=//);
	(length($str) * 3 / 4) - $padding;
}

our @patterns; my $pattern_i = -1;
my %facets = (
	length => sub {
		my ($o, $var) = @_;
		return unless exists $o->{length};
		sprintf('length(%s)==%d', $var, delete $o->{length});
	},
	maxLength => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxLength};
		sprintf('length(%s)<=%d', $var, delete $o->{maxLength});
	},
	minLength => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minLength};
		sprintf('length(%s)>=%d', $var, delete $o->{minLength});
	},
	lengthHex => sub {
		my ($o, $var) = @_;
		return unless exists $o->{length};
		sprintf('Types::XSD::hex_length(%s)==%d', $var, delete $o->{length});
	},
	maxLengthHex => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxLength};
		sprintf('Types::XSD::hex_length(%s)<=%d', $var, delete $o->{maxLength});
	},
	minLengthHex => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minLength};
		sprintf('Types::XSD::hex_length(%s)>=%d', $var, delete $o->{minLength});
	},
	lengthQName => sub {
		my ($o, $var) = @_;
		return unless exists $o->{length};
		delete $o->{length};
		"!!1"
	},
	maxLengthQName => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxLength};
		delete $o->{maxLength};
		"!!1"
	},
	minLengthQName => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minLength};
		delete $o->{minLength};
		"!!1"
	},
	lengthB64 => sub {
		my ($o, $var) = @_;
		return unless exists $o->{length};
		sprintf('Types::XSD::b64_length(%s)==%d', $var, delete $o->{length});
	},
	maxLengthB64 => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxLength};
		sprintf('Types::XSD::b64_length(%s)<=%d', $var, delete $o->{maxLength});
	},
	minLengthB64 => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minLength};
		sprintf('Types::XSD::b64_length(%s)>=%d', $var, delete $o->{minLength});
	},
	pattern => sub {
		my ($o, $var) = @_;
		return unless exists $o->{pattern};
		$patterns[++$pattern_i] = delete $o->{pattern};
		sprintf('%s =~ $Types::XSD::patterns[%d]', $var, $pattern_i);
	},
	enumeration => sub {
		my ($o, $var) = @_;
		return unless exists $o->{enumeration};
		my $re = join "|", map quotemeta, @{delete $o->{enumeration}};
		sprintf('%s =~ m/^(?:%s)$/sm', $var, $re);
	},
	whiteSpace => sub {
		my ($o, $var) = @_;
		return unless exists $o->{whiteSpace};
		delete($o->{whiteSpace});
		"!!1";
	},
	maxInclusive => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxInclusive};
		quick_range_check("Math::BigInt", undef, delete($o->{maxInclusive}), undef, undef, $var);
	},
	minInclusive => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minInclusive};
		quick_range_check("Math::BigInt", delete($o->{minInclusive}), undef, undef, undef, $var);
	},
	maxExclusive => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxExclusive};
		quick_range_check("Math::BigInt", undef, delete($o->{maxExclusive}), undef, 1, $var);
	},
	minExclusive => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minExclusive};
		quick_range_check("Math::BigInt", delete($o->{minExclusive}), undef, 1, undef, $var);
	},
	maxInclusiveFloat => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxInclusive};
		quick_range_check("Math::BigFloat", undef, delete($o->{maxInclusive}), undef, undef, $var);
	},
	minInclusiveFloat => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minInclusive};
		quick_range_check("Math::BigFloat", delete($o->{minInclusive}), undef, undef, undef, $var);
	},
	maxExclusiveFloat => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxExclusive};
		quick_range_check("Math::BigFloat", undef, delete($o->{maxExclusive}), undef, 1, $var);
	},
	minExclusiveFloat => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minExclusive};
		quick_range_check("Math::BigFloat", delete($o->{minExclusive}), undef, 1, undef, $var);
	},
	maxInclusiveStr => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxInclusive};
		sprintf('%s le %s', $var, perlstring delete $o->{maxInclusive});
	},
	minInclusiveStr => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minInclusive};
		sprintf('%s ge %s', $var, perlstring delete $o->{minInclusive});
	},
	maxExclusiveStr => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxExclusive};
		sprintf('%s lt %s', $var, perlstring delete $o->{maxExclusive});
	},
	minExclusiveStr => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minExclusive};
		sprintf('%s gt %s', $var, perlstring delete $o->{minExclusive});
	},
	maxInclusiveDuration => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxInclusive};
		sprintf('(Types::XSD::dur_cmp(%s, %s)||0) <= 0', $var, perlstring delete $o->{maxInclusive});
	},
	minInclusiveDuration => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minInclusive};
		sprintf('(Types::XSD::dur_cmp(%s, %s)||0) >= 0', $var, perlstring delete $o->{minInclusive});
	},
	maxExclusiveDuration => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxExclusive};
		sprintf('(Types::XSD::dur_cmp(%s, %s)||0) < 0', $var, perlstring delete $o->{maxExclusive});
	},
	minExclusiveDuration => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minExclusive};
		sprintf('(Types::XSD::dur_cmp(%s, %s)||0) > 0', $var, perlstring delete $o->{minExclusive});
	},
	maxInclusiveDT => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxInclusive};
		sprintf('(Types::XSD::dt_cmp(%s, %s, %s)||0) <= 0', perlstring($T), $var, perlstring delete $o->{maxInclusive});
	},
	minInclusiveDT => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minInclusive};
		sprintf('(Types::XSD::dt_cmp(%s, %s, %s)||0) >= 0', perlstring($T), $var, perlstring delete $o->{minInclusive});
	},
	maxExclusiveDT => sub {
		my ($o, $var) = @_;
		return unless exists $o->{maxExclusive};
		sprintf('(Types::XSD::dt_cmp(%s, %s, %s)||0) < 0', perlstring($T), $var, perlstring delete $o->{maxExclusive});
	},
	minExclusiveDT => sub {
		my ($o, $var) = @_;
		return unless exists $o->{minExclusive};
		sprintf('(Types::XSD::dt_cmp(%s, %s, %s)||0) > 0', perlstring($T), $var, perlstring delete $o->{minExclusive});
	},
	totalDigits => sub {
		my ($o, $var) = @_;
		return unless exists $o->{totalDigits};
		sprintf('do { no warnings "uninitialized"; my $tmp = %s; ($tmp=~tr/0-9//) <= %d }', $var, delete $o->{totalDigits});
	},
	fractionDigits => sub {
		my ($o, $var) = @_;
		return unless exists $o->{fractionDigits};
		sprintf('do { no warnings "uninitialized"; my (undef, $tmp) = split /\\./, %s; ($tmp=~tr/0-9//) <= %d }', $var, delete $o->{fractionDigits});
	},
);

sub facet
{
	my $self   = pop;
	my @facets = @_;
	my $regexp = qr{^${\(join "|", map quotemeta, @facets)}$}ms;
	my $name   = "$self";
	
	my $inline_generator = sub
	{
		my %p = @_;
		return sub {
			local $T = $_[0]->parent;
			my $var  = $_[1];
			my $r    = sprintf(
				'(%s)',
				join(
					' and ',
					$self->inline_check($var),
					map($facets{$_}->(\%p, $var), @facets),
				),
			);
			croak sprintf(
				'Attempt to parameterize type "%s" with unrecognised parameter%s %s',
				$name,
				scalar(keys %p)==1 ? '' : 's',
				join(", ", map(qq["$_"], sort keys %p)),
			) if keys %p;
			return $r;
		};
	};
	
	$self->{inline_generator} = $inline_generator;
	$self->{constraint_generator} = sub {
		my $sub = sprintf(
			'sub { %s }',
			$inline_generator->(@_)->($self, '$_[0]'),
		);
		eval($sub) or croak "could not build sub: $@\n\nCODE: $sub\n";
	};
	$self->{name_generator} = sub {
		my ($s, %a) = @_;
		sprintf('%s[%s]', $s, join q[,], map sprintf("%s=>%s", $_, perlstring $a{$_}), sort keys %a);
	};
	
	return if $self->is_anon;
	
	no strict qw( refs );
	no warnings qw( redefine prototype );
	*{$self->name} = __PACKAGE__->_mksub($self);
}

our @dtarr;
my $i = -1;
our $base_datetime = "DateTime"->new(year => 2000, month => 1, day => 1); # leap year, 31 day month
our %dt_regexps;
sub dt_maker
{
	my ($name, $regexp, @fields) = @_;
	my $j = ++$i; $dtarr[$j] = $regexp;
	
	my $inlined = sub
	{
		my $var = $_[1];
		my @code;
		push @code, "do {";
		push @code, sprintf(
			'my (%s) = (%s =~ $Types::XSD::dtarr[%d]);',
			join(', ', map "\$$_", @fields),
			$var,
			$j,
		);
		push @code, sprintf(
			'eval { "DateTime::Incomplete"->new(%s)->to_datetime(base => $Types::XSD::base_datetime) };',
			join(', ', map "$_ => \$$_", @fields),
		);
		push @code, "}";
		"@code";
	};
	
	my $type = "Type::Tiny"->new(
		name       => $name,
		library    => __PACKAGE__,
		constraint => eval sprintf('sub { %s }', $inlined->(undef, '$_')),
		inlined    => $inlined,
	);
	__PACKAGE__->add_type($type);
	
	facet(
		qw( pattern whiteSpace enumeration maxInclusiveDT maxExclusiveDT minInclusiveDT minExclusiveDT ),
		$type,
	);
	
	$dt_regexps{$type} = [$regexp, @fields];
}

sub dt_parse
{
	my ($type, $a) = @_;
	my ($re, @fields) = @{ $dt_regexps{$type} };
	my %d;
	@d{@fields} = ($a =~ $re);
	!defined($d{$_}) && delete($d{$_}) for @fields;
	"DateTime::Incomplete"->new(%d);
}

sub dur_parse
{
	goto \&DateTimeX::Auto::dur;
}

{
	my %cache;
	sub dt_cmp
	{
		my ($type, $a, $b) = @_;
		$type = __PACKAGE__->get_type($type) unless ref $type;
		my $A = eval($cache{"$type;a"} ||= $type->inline_check('$a'));
		my $B = eval($cache{"$type;b"} ||= $type->inline_check('$b'));
		$A <=> $B;
	}
}

use Types::Standard;
use Type::Utils;
use Type::Library -base, -declare => qw(
	AnyType AnySimpleType String NormalizedString Token Language Name
	NmToken NmTokens NCName Id IdRef IdRefs Entity Entities Boolean
	Base64Binary HexBinary Float Double AnyURI QName Notation Decimal
	Integer NonPositiveInteger NegativeInteger Long Int Short Byte
	NonNegativeInteger PositiveInteger UnsignedLong UnsignedInt
	UnsignedShort UnsignedByte Duration DateTime Time Date GYearMonth
	GYear GMonthDay GDay GMonth
);

our @EXPORT_OK = qw( dt_cmp dur_cmp dt_parse dur_parse );

declare AnyType, as Types::Standard::Any;

declare AnySimpleType, as Types::Standard::Value;

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare String, as Types::Standard::Str;

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare NormalizedString, as Types::Standard::StrMatch[qr{^[^\t\r\n]*$}sm];

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare Token, as intersection([
	NormalizedString,
	Types::Standard::StrMatch([qr{^\s}sm])->complementary_type,
	Types::Standard::StrMatch([qr{\s$}sm])->complementary_type,
	Types::Standard::StrMatch([qr{\s{2}}sm])->complementary_type,
]);

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare Language, as Types::Standard::StrMatch[qr{^[a-zA-Z]{1,8}(?:-[a-zA-Z0-9]{1,8})*$}sm];

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare Name, as Types::Standard::StrMatch[qr{^(?:$XML::RegExp::Name)$}sm];

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare NmToken, as Types::Standard::StrMatch[qr{^(?:$XML::RegExp::NmToken)$}sm];

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare NmTokens, as Types::Standard::StrMatch[qr{^(?:$XML::RegExp::NmToken)(?:\s+$XML::RegExp::NmToken)*$}sm];

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare NCName, as Types::Standard::StrMatch[qr{^(?:$XML::RegExp::NCName)$}sm];

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare Id, as NCName;

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare IdRef, as NCName;

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare IdRefs, as Types::Standard::StrMatch[qr{^(?:$XML::RegExp::NCName)(?:\s+$XML::RegExp::NCName)*$}sm];

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare Entity, as NCName;

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare Entities, as Types::Standard::StrMatch[qr{^(?:$XML::RegExp::NCName)(?:\s+$XML::RegExp::NCName)*$}sm];

facet qw( pattern whiteSpace ),
declare Boolean, as Types::Standard::StrMatch[qr{^(?:true|false|0|1)$}ism];

facet qw( lengthB64 minLengthB64 maxLengthB64 pattern enumeration whiteSpace ),
declare Base64Binary, as Types::Standard::StrMatch[qr{^[a-zA-Z0-9+\x{2f}=\s]+$}ism];

facet qw( lengthHex minLengthHex maxLengthHex pattern enumeration whiteSpace ),
declare HexBinary, as Types::Standard::StrMatch[qr{^[a-fA-F0-9]+$}ism];

facet qw( pattern enumeration whiteSpace maxInclusiveFloat maxExclusiveFloat minInclusiveFloat minExclusiveFloat ),
declare Float, as Types::Standard::Num;

facet qw( pattern enumeration whiteSpace maxInclusiveFloat maxExclusiveFloat minInclusiveFloat minExclusiveFloat ),
declare Double, as Types::Standard::Num;

facet qw( length minLength maxLength pattern enumeration whiteSpace ),
declare AnyURI, as Types::Standard::Str;

facet qw( lengthQName minLengthQName maxLengthQName pattern enumeration whiteSpace ),
declare QName, as Types::Standard::StrMatch[qr{^(?:$XML::RegExp::QName)$}sm];

facet qw( lengthQName minLengthQName maxLengthQName pattern enumeration whiteSpace ),
declare Notation, as QName;

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusiveFloat maxExclusiveFloat minInclusiveFloat minExclusiveFloat ),
declare Decimal, as Types::Standard::StrMatch[qr{^(?:(?:[+-]?[0-9]+(?:\.[0-9]+)?)|(?:[+-]?\.[0-9]+))$}ism];

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare Integer, as Types::Standard::Int;

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare NonPositiveInteger, as Integer, create_range_check("Math::BigInt", undef, 0);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare NegativeInteger, as NonPositiveInteger, create_range_check("Math::BigInt", undef, -1);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare NonNegativeInteger, as Integer, create_range_check("Math::BigInt", 0, undef);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare PositiveInteger, as NonNegativeInteger, create_range_check("Math::BigInt", 1, undef);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare Long, as Integer, create_range_check("Math::BigInt", q[-9223372036854775808], q[9223372036854775807]);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare Int, as Long, create_range_check("Math::BigInt", q[-2147483648], q[2147483647]);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare Short, as Int, create_range_check("Math::BigInt", q[-32768], q[32767]);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare Byte, as Short, create_range_check("Math::BigInt", q[-128], q[127]);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare UnsignedLong, as NonNegativeInteger, create_range_check("Math::BigInt", q[0], q[18446744073709551615]);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare UnsignedInt, as UnsignedLong, create_range_check("Math::BigInt", q[0], q[4294967295]);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare UnsignedShort, as UnsignedInt, create_range_check("Math::BigInt", q[0], q[65535]);

facet qw( totalDigits fractionDigits pattern whiteSpace enumeration maxInclusive maxExclusive minInclusive minExclusive ),
declare UnsignedByte, as UnsignedShort, create_range_check("Math::BigInt", q[0], q[255]);

facet qw( pattern whiteSpace enumeration maxInclusiveDuration maxExclusiveDuration minInclusiveDuration minExclusiveDuration ),
declare Duration, as Types::Standard::StrMatch[
	qr{^P
		(?:[0-9]+Y)?
		(?:[0-9]+M)?
		(?:[0-9]+D)?
		(?:T
			(?:[0-9]+H)?
			(?:[0-9]+M)?
			(?:[0-9]+(?:\.[0-9]+)?S)?
		)?
	$}xism
];

dt_maker(
	DateTime => qr{^
		(-?[0-9]{4,})
		-
		([0-9]{2})
		-
		([0-9]{2})
		T
		([0-9]{2})
		:
		([0-9]{2})
		:
		([0-9]{2}(?:\.[0-9]+)?)
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( year month day hour minute second time_zone ),
);

dt_maker(
	Time => qr{^
		([0-9]{2})
		:
		([0-9]{2})
		:
		([0-9]{2}(?:\.[0-9]+)?)
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( hour minute second time_zone ),
);

dt_maker(
	Date => qr{^
		(-?[0-9]{4,})
		-
		([0-9]{2})
		-
		([0-9]{2})
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( year month day time_zone ),
);

dt_maker(
	GYearMonth => qr{^
		(-?[0-9]{4,})
		-
		([0-9]{2})
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( year month time_zone ),
);

dt_maker(
	GYear => qr{^
		(-?[0-9]{4,})
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( year time_zone ),
);

dt_maker(
	GMonthDay => qr{^
		-
		-
		([0-9]{2})
		-
		([0-9]{2})
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( month day time_zone ),
);

dt_maker(
	GDay => qr{^
		-
		-
		-
		([0-9]{2})
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( day time_zone ),
);

dt_maker(
	GMonth => qr{^
		-
		-
		([0-9]{2})
		(Z | (?: [+-]\d{2}:?\d{2} ))?
	$}xism,
	qw( month time_zone ),
);

1;

__END__

=pod

=encoding utf-8

=head1 NAME

Types::XSD - type constraints based on XML schema datatypes

=head1 SYNOPSIS

   package Person;
   
   use Moo;
   use Types::XSD qw( PositiveInteger String );
   
   has name => (is => "ro", isa => String[ minLength => 1 ]);
   has age  => (is => "ro", isa => PositiveInteger);

=head1 DESCRIPTION

=head2 Type Constraints

This module defines the following type constraints based on the data types
defined in L<XML Schema|http://www.w3.org/TR/xmlschema-2/>. (The names of
the type constraints are the same as the XML Schema data types, but
capitalization often differs.)

=over

=item C<< AnyType >>

=item C<< AnySimpleType >>

=item C<< String >>

=item C<< NormalizedString >>

=item C<< Token >>

=item C<< Language >>

=item C<< Name >>

=item C<< NmToken >>

=item C<< NmTokens >>

=item C<< NCName >>

=item C<< Id >>

=item C<< IdRef >>

=item C<< IdRefs >>

=item C<< Entity >>

=item C<< Entities >>

=item C<< Boolean >>

=item C<< Base64Binary >>

=item C<< HexBinary >>

=item C<< Float >>

=item C<< Double >>

=item C<< AnyURI >>

=item C<< QName >>

=item C<< Notation >>

=item C<< Decimal >>

=item C<< Integer >>

=item C<< NonPositiveInteger >>

=item C<< NegativeInteger >>

=item C<< Long >>

=item C<< Int >>

=item C<< Short >>

=item C<< Byte >>

=item C<< NonNegativeInteger >>

=item C<< PositiveInteger >>

=item C<< UnsignedLong >>

=item C<< UnsignedInt >>

=item C<< UnsignedShort >>

=item C<< UnsignedByte >>

=item C<< Duration >>

=item C<< DateTime >>

=item C<< Time >>

=item C<< Date >>

=item C<< GYearMonth >>

=item C<< GYear >>

=item C<< GMonthDay >>

=item C<< GDay >>

=item C<< GMonth >>

=back

Datatypes can be parameterized using the facets defined by XML Schema. For
example:

   use Types::XSD qw( String Decimal PositiveInteger Token );
   
   my @sizes = qw( XS S M L XL XXL );
   
   has name   => (is => "ro", isa => String[ minLength => 1 ]);
   has price  => (is => "ro", isa => Decimal[ fractionDigits => 2 ]);
   has rating => (is => "ro", isa => PositiveInteger[ maxInclusive => 5 ]);
   has size   => (is => "ro", isa => Token[ enumeration => \@sizes ]);

The C<whiteSpace> facet is ignored as I'm not entirely sure what it should
do. It perhaps makes sense for coercions, but this module doesn't define any
coercions.

Note that to be super-correct, the C<< {max,min}{Inclusive,Exclusive} >>
facets for numeric types are performed by passing the numbers through
L<Math::BigInt> or L<Math::BigFloat>, so may be a little slow.

=head2 Functions

This module also exports some convenience functions:

=over

=item C<< dur_parse($str) >>

Parse an xsd:duration string, returning a L<DateTime::Duration>.

=item C<< dur_cmp($a, $b) >>

Compare two strings conforming to the xsd:duration datatype to indicate
which is the longer duration.

Returns -1 if $a is shorter. Returns 1 if $b is shorter. Returns 0 if the
durations are identical. Returns undef if the comparison is indeterminate;
for example, "P1Y" (one year) and "P365D" (365 days) are not necessarily
identical - in leap years "P365D" is shorter.

=item C<< dt_cmp($type, $a, $b) >>

Compare two datetime-like strings. For example, two C<gYearMonth> strings
can be compared using:

   dt_cmp(GYearMonth, "2009-02", "2010-10");

Both strings are expected to conform to the same datatype. It doesn't make
much sense to compare them otherwise.

=item C<< dt_parse($type, $str) >>

Parse a datetime-like string, returning a L<DateTime::Incomplete> object.
Note that L<DateTime::Incomplete> objects are always returned, even if the
datetime is potentially complete.

=back

=head1 BUGS

Please report any bugs to
L<http://rt.cpan.org/Dist/Display.html?Queue=Types-XSD>.

=head1 SEE ALSO

L<Type::Tiny>, L<Types::Standard>.

=head1 AUTHOR

Toby Inkster E<lt>tobyink@cpan.orgE<gt>.

=head1 COPYRIGHT AND LICENCE

This software is copyright (c) 2013 by Toby Inkster.

This is free software; you can redistribute it and/or modify it under
the same terms as the Perl 5 programming language system itself.

=head1 DISCLAIMER OF WARRANTIES

THIS PACKAGE IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.

