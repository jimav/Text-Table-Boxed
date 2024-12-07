#!/usr/bin/perl
use FindBin qw($Bin);
use lib $Bin, "$Bin/../lib";
use t_Common qw/oops/; # strict, warnings, Carp
use t_TestCommon # Test2::V0 etc.
  qw/$silent $verbose $debug run_perlscript/;

use Text::Table::Boxed;

{ my $tb = Text::Table::Boxed->new("aaa","bbb","ccc");
  is($tb, <<'EOF', "title-only table");
+-----------------+
| aaa | bbb | ccc |
+=================+
EOF
}


{ my $tb = Text::Table::Boxed->new();
  is($tb, <<'EOF', "empty table");
+-----------+
+-----------+
EOF
}

done_testing;

