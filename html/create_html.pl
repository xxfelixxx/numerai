#!/usr/bin/env perl

use warnings;
use strict;

my @files = map { chomp; $_ } qx(ls -f1 | grep feature | grep png);

my %types = ( '' => 1 ); # feature1.png
my %names;
for my $file (@files) {
    my ($name, $type) = $file =~ m|^(feature\d+)(\D*)\.png$|;
    $names{$name}++;
    next unless $type;
    $types{$type}++;
}

create_index();
print "Created index.html\n";


open my $fh, ">table.html";

print_header();
my @ordered = map  { "feature$_" }
              sort { $a <=> $b}
              map  { s|feature||; $_ }
              keys %names;
for my $name (@ordered) {
    print_row($name, sort keys %types);
}
print_footer();
print "Updated table.html\n";

exit 0;

sub print_header {

    print $fh <<EOHEADER;
<html>
<style type="text/css">
.tg  {border-collapse:collapse;border-spacing:0;}
.tg td{font-family:Arial, sans-serif;font-size:12px;padding:5px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;}
.tg th{font-family:Arial, sans-serif;font-size:12px;font-weight:normal;padding:5px 5px;border-style:solid;border-width:1px;overflow:hidden;word-break:normal;}
.tg .tg-yw4l{vertical-align:top}
</style>
<body>
<table class="tg">
EOHEADER
}

sub print_row {
    my ($name, @types) = @_;
    print $fh "  <tr>\r\n";
    for my $type (@types) {
        my $filename = join('', $name, $type, '.png');
        print $fh '    <td class="tg-yw4l"><a href="' . $filename . '" target="image" width="100%" height="100%">' . $filename . '</a></td>' . "\n";
    }
    print $fh "  </tr>" . "\n";
}

sub print_footer {
    print $fh <<EOFOOTER;
</table>

</body>
</html>
EOFOOTER
}

sub create_index {
    open my $ind, ">index.html";
    print $ind <<EOINDEX;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"
   "http://www.w3.org/TR/html4/frameset.dtd">
<HTML>
<HEAD>
<TITLE>NumerAI Feature Set</TITLE>
</HEAD>
<FRAMESET cols="40%, 60%">
  <FRAMESET>
      <FRAME name="table" src="table.html">
  </FRAMESET>
  <FRAME name="image" src="feature1.png">
</FRAMESET>
</HTML>
EOINDEX

    close $ind;
}
