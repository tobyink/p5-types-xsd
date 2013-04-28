use inc::Module::Package 'RDF:tobyink 0.012';

use strict;
use warnings;

if (-d ".hg") # I am the author
{
	use lib "lib";
	require Types::XSD;
	if ("Types::XSD"->VERSION =~ /_/) # only for dev releases
	{
		my $SOURCE = "../p5-type-tiny/lib";
		my $DEST   = "inc";
		
		require PIR;
		my $iter = "PIR"->new->file->name("*.pm")->iter($SOURCE, { relative => 1 });
		while (my $module = $iter->())
		{
			"Module::Install::Admin"->copy("$SOURCE/$module" => "$DEST/$module");
		}
	}
}
