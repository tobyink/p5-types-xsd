use v5.14;

use B qw(perlstring);
use Data::Dumper;
use Path::Iterator::Rule;
use Path::Tiny qw(path);
use Types::XSD;
use XML::LibXML;

my $tests = "XML::LibXML"->load_xml(location => "devel.w3c-testing/testset.xml")->documentElement;
my %files;

for my $group ($tests->getChildrenByTagName("testGroup"))
{
	my ($type) = ($group->{name} =~ /^atomic-(\w+?)-/) or next;
	
	my $out;
	$files{$type} ||= do {
		$out = path("t/50$type.t")->openw_utf8;
		print $out <<'OUT';
use strict;
use warnings;
use utf8;

use Test::DescribeMe qw(extended);
use Test::More;
use Test::TypeTiny;

use Types::XSD;

sub mk_type { "Types::XSD"->get_type($_[0])->parameterize(%{$_[1]}) }

OUT
		$out;
	};
	$out = $files{$type};
	
	my $doc = $group->getElementsByTagName('documentation')->get_node(1)->textContent;
	$doc =~ s/(^\s*)|(\s*$)//gsm;
	
	printf $out
		"subtest %s => sub {\n",
		perlstring($doc),
	;
	
	my $schema_file = sprintf(
		'devel.w3c-testing/tests/%s/%s.xsd',
		$type,
		$group->getElementsByTagName('schemaTest')->get_node(1)->{name},
	);
	my $schema_xml = "XML::LibXML"->load_xml(location => $schema_file);
	my ($schema_restriction) = $schema_xml->getElementsByTagName('xs:restriction');
	
	my $base_type = $schema_restriction->{base};
	my %facets;
	for ($schema_restriction->getChildrenByTagName('*'))
	{
		$_->tagName eq 'xs:enumeration'
			? push(@{$facets{substr($_->tagName,3)}}, $_->{value})
			: ($facets{substr($_->tagName,3)} = $_->{value});
	}
	
	my ($constraint) = grep lc("xs:$_") eq lc($base_type), "Types::XSD"->type_names;
	if ( exists $facets{pattern} )
	{
		# XML regexes have these \c and \i things which we'll attempt to fake.
		$facets{pattern} =~ s/\\c/(?:\$XML::RegExp::NameChar)/g;
		$facets{pattern} =~ s/\\i/(?:\$XML::RegExp::NameChar)/g;
		eval { $facets{pattern} = qr{^$facets{pattern}$}ms }
		or do {
			print $out "\tlocal \$TODO = \"could not compile regexp\";\n";
			delete($facets{pattern});
		};
	}
	
	local $Data::Dumper::Terse  = 1;
	local $Data::Dumper::Indent = 0;
	
	printf $out "\tmy \$type = mk_type(%s, %s);\n", Dumper($constraint, \%facets);
	
	for my $inst ($group->getElementsByTagName('instanceTest'))
	{
		my $inst_file = sprintf(
			'devel.w3c-testing/tests/%s/%s.xml',
			$type,
			$inst->{name},
		);
		my $inst_xml = "XML::LibXML"->load_xml(location => $inst_file);
		my $value = $inst_xml->documentElement->textContent;
		
		printf $out
			"\t%s(%s, \$type, %s);\n",
			$inst->getChildrenByTagName("expected")->[0]->{validity} eq 'valid' ? 'should_pass' : 'should_fail',
			perlstring($value),
			perlstring($inst->{name}),
		;
	}
	
	printf $out "\tdone_testing;\n";
	printf $out "};\n\n";
}

for my $h (values %files)
{
	print $h "done_testing;\n\n";
	close $h;
}
