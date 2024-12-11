#!/usr/bin/perl
use FindBin qw($Bin);
use lib $Bin, "$Bin/../lib";
use t_Common qw/oops btw btwN/; # strict, warnings, Carp
use t_TestCommon # Test2::V0 etc.
  qw/$silent $verbose $debug run_perlscript/;

use feature 'signatures';

use Text::Table::Boxed;
$Text::Table::Boxed::debug = $debug;

use Data::Dumper::Interp;
{ no warnings 'once';
  # Don't follow overloads e.g. stringify
  $Data::Dumper::Interp::Objects = {overloads => "ignore"};
}

sub cx2let :prototype(_) {
  my $cx = shift;
  my $ABC="A"; ++$ABC for (1..$cx); return $ABC
}

sub mk_table(%opts) {
  my $num_data_cols = delete($opts{num_data_cols}) // 3;
  my $num_pic_cols  = delete($opts{num_pic_cols}) // ($num_data_cols < 3 ? 2 : 3);
  my $num_pic_rows  = delete($opts{num_pic_rows}) // 2;
  my $num_body_rows = delete($opts{num_body_rows}) // 1;
  my $nobox         = delete($opts{nobox});
  my $nopad         = delete($opts{nopad});
  foreach (keys %opts) { oops "Wrong key '$_'" }

  #   normal:      nopad:     nobox:      nopad & nobox:
  #   +-------+    +---+
  #   | c | c |    |c|c|      "c | c"         "c|c"
  #   +-------+    +---+
  my $picwidth = ($num_pic_cols*($nopad ? 2 : 4) - 1)
                 + ($nobox ? 0 : 2);

  my sub _mkline($lhs, $colstr, $rhs, $padch=" ", $sep="|") {
     #($nobox ? "" : $lhs.($nopad ? "" : " "))
     ($nobox ? "" : $lhs)
    .join($sep, (($nopad ? $colstr : "${padch}${colstr}${padch}") x $num_pic_cols))
    #.($nobox ? "" : ($nopad ? "" : " ").$rhs)
    .($nobox ? "" : $rhs)
    ."\n"
  }
  my $datarow_line = _mkline("|", "c", "|");
  my $top_line     = _mkline("[", "-", "]", "-", "+");
  my $aftert_line  = _mkline("+", "T", "+", "T", "+");
  my $midsep_line  = _mkline("+", "m", "+", "m", "+");
  my $bot_line     = _mkline("<", "b", ">", "b", "+");
  my @picture = (
      ($nobox ? () : ($top_line)),
      ($num_pic_rows == 0 ? () :
        ( $datarow_line,
          ($num_pic_rows >= 3 ? $aftert_line :
           $num_pic_rows == 2 ? $midsep_line : ()
          ),
          (map{ ($datarow_line, $midsep_line) } 2..$num_pic_rows-1
          ),
          ($num_pic_rows >= 2 ? ($datarow_line) : ()),
        )
      ),
      ($nobox ? () : ($bot_line)),
  );
  my $tb = Text::Table::Boxed->new({
      columns => [ map{ "Title-A$_" } 1..$num_data_cols ],
      picture => \@picture
  });
  # Add data rows
  for my $brx (0..$num_body_rows-1) {
    $tb->add(map{
               my $str = "Data-".cx2let($brx+1).($_+1); # e.g. Data-B1
               $_ == 0 ? $str :
               $_ == 1 ? join("", $str, map{"\nextra wide line$_"} 2..$_+1) :
                         join("", $str, map{"\nline$_"} 2..$_+1)
             }0..$num_data_cols-1
            );
  }
  return $tb
}
#{
#  #my $tb = mk_table(num_body_rows => 3, num_pic_rows => 7, num_pic_cols => 5);
#  my $tb = mk_table(num_body_rows => 0);
#  print "PICTURE:\n", @{ $tb->{Text::Table::Boxed::MYKEY()}->{picture} },"\n";
#  print $tb;
#}

# Preliminary test using rules with all-different characters 
{ my $tb = Text::Table::Boxed->new({ columns => ["aaa","bbb"],
                                     picture => <<'EOF'
(*%!)
c | c
<@^=>
EOF
                                    });
  $tb->add("aaaval","bv");
  is($tb->table(0,1), "aaa    | bbb\n", "table(0,1)");
  is($tb->table(1,1), "aaaval | bv \n", "table(1,1)");
  is($tb->top_rule(), "((((((*%!)))\n", "top_rule");
  is($tb->bot_rule(), "<<<<<<@^=>>>\n", "bot_rule");
  is([$tb->rows(0,1)], [["aaa    | bbb\n"]], "rows(0,1)");
  is([$tb->rows(0,2)], [ ["aaa    | bbb\n"], ["aaaval | bv \n"] ], "rows(0,2)");
  is([$tb->body_rows(0,1)], [ ["aaaval | bv \n"] ], "body_rows(0,1)");
  is([$tb->rendered_body_rows(0,1)], 
     [ ["aaaval | bv \n", "<<<<<<@^=>>>\n"] ], "rendered_body_rows(0,1)");
  is($tb, <<EOF, "stringify special table");
((((((*%!)))
aaa    | bbb
aaaval | bv\x{20}
<<<<<<@^=>>>
EOF
}

{ my $num_body_rows = 1;
  for my $num_pic_cols (2..5) {
    for my $num_pic_rows(2..5) {
      my $tb = mk_table(num_body_rows => $num_body_rows,
                        num_data_cols => 2, 
                        num_pic_cols => $num_pic_cols, 
                        num_pic_rows => $num_pic_rows);
      #print "PICTURE:\n", @{ $tb->{Text::Table::Boxed::MYKEY()}->{picture} },"\n";
      my $aftertitle = "+TTTTTTTTTT+TTTTTTTTTTTTTTTTTT+";
      $aftertitle =~ s/T/m/g if $num_pic_rows <= 2;
      is($tb, <<EOF, "nbr=1 ndc=2 num_pic_cols=$num_pic_cols _rows=$num_pic_rows")
[----------+------------------]
| Title-A1 | Title-A2         |
$aftertitle
| Data-B1  | Data-B2          |
|          | extra wide line2 |
<bbbbbbbbbb+bbbbbbbbbbbbbbbbbb>
EOF
   }
  }

  for my $num_pic_cols (3..8) {
    for my $num_pic_rows(2..8) {
      my $tb = mk_table(num_body_rows => $num_body_rows,
                        num_data_cols => 5, 
                        num_pic_cols => $num_pic_cols, 
                        num_pic_rows => $num_pic_rows);
      my $aftertitle = "+TTTTTTTTTT+TTTTTTTTTTTTTTTTTT+TTTTTTTTTT+TTTTTTTTTT+TTTTTTTTTT+";
      $aftertitle =~ s/T/m/g if $num_pic_rows <= 2;
      is($tb, <<EOF, "nbr=1 ndc=5 num_pic_cols=$num_pic_cols _rows=$num_pic_rows")
[----------+------------------+----------+----------+----------]
| Title-A1 | Title-A2         | Title-A3 | Title-A4 | Title-A5 |
$aftertitle
| Data-B1  | Data-B2          | Data-B3  | Data-B4  | Data-B5  |
|          | extra wide line2 | line2    | line2    | line2    |
|          |                  | line3    | line3    | line3    |
|          |                  |          | line4    | line4    |
|          |                  |          |          | line5    |
<bbbbbbbbbb+bbbbbbbbbbbbbbbbbb+bbbbbbbbbb+bbbbbbbbbb+bbbbbbbbbb>
EOF
    }
  }
}

{ my $tb = Text::Table::Boxed->new();
  is($tb, "", "empty table");
}

{ my $tb = mk_table(num_body_rows => 0, num_data_cols => 3, num_pic_cols => 3);
  #print "PICTURE:\n", @{ $tb->{Text::Table::Boxed::MYKEY()}->{picture} },"\n";
  is($tb, <<'EOF', "zaro body rows");
[----------+----------+----------]
| Title-A1 | Title-A2 | Title-A3 |
<bbbbbbbbbb+bbbbbbbbbb+bbbbbbbbbb>
EOF
}

done_testing;
