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
my $fh;

create_summary();
print "Created summary.html\n";

create_summary_links(sort keys %types);
print "Created summary_links.html\n";

for my $type (keys %types) {
    create_summary_page($type);
}

exit 0;

sub create_summary_page {
    my ($type) = @_;
    my $html = 'feature' . $type . '_summary.html';
    open $fh, ">$html";

    print_header();
    my @ordered = map  { "feature$_" }
                  sort { $a <=> $b}
                  map  { s|feature||; $_ }
                  keys %names;
    print_rows($type, @ordered);
    print_footer();

    print "Created $html\n";

}

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

sub print_rows {
    my ($type, @names) = @_;

    # TODO: use factor to get rows/columns right
    my $cols = 4;
    my $count = 0;
    my $percent = '"' . int(100/$cols) . '%' . '"';
    $percent = '"100%"';
    for my $name (@names) {
        print $fh "  <tr>\r\n" unless $count;
        my $filename = join('', $name, $type, '.png');
        print $fh '    <td class="tg-yw4l"><img src="' . $filename;
        print $fh '" width=' . $percent . ' height=' . $percent . '></td>' . "\n";
        print $fh "  </tr>" . "\n" if $count % $cols == $cols-1;
        $count++;
        
    }
}

sub print_footer {
    print $fh <<EOFOOTER;
</table>

</body>
</html>
EOFOOTER

    close $fh;
}

sub create_summary {
    open my $ind, ">summary.html";
    print $ind <<EOINDEX;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"
   "http://www.w3.org/TR/html4/frameset.dtd">
<HTML>
<HEAD>
<TITLE>NumerAI Feature Summary</TITLE>
</HEAD>
<FRAMESET rows="10%, 90%">
  <FRAMESET>
      <FRAME name="links" src="summary_links.html">
  </FRAMESET>
  <FRAME name="summary" src="feature_summary.html">
</FRAMESET>
</HTML>
EOINDEX

    close $ind;
}

sub create_summary_links {
    my (@names) = @_;

    open my $ind, ">summary_links.html";
    print $ind <<EOHEAD;
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Frameset//EN"
   "http://www.w3.org/TR/html4/frameset.dtd">
<html>
<head>
<title>NumerAI Feature Summary</title>
</head>
<body>
<center>
<table width="100%" border="0"><tr>
EOHEAD

    for my $name (@names) {
        my $summary = 'feature' . $name . '_summary.html';
        my $label = "$name summary";
        $label =~ s|_| |g;
        $label =~ s|^\s*||;
        print $ind '<td align="center"><a href="' . $summary . '" target="summary">' . $label . '</a></td>' . "\n";
    }

    print $ind <<EOFOOT;
</tr>
</table>
</center>
</body>
</html>
EOFOOT

    close $ind;
}
