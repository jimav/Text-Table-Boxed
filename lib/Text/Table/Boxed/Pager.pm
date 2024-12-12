# License: http://creativecommons.org/publicdomain/zero/1.0/
# (CC0 or Public Domain).  To the extent possible under law, the author,
# Jim Avera (email jim.avera at gmail) has waived all copyright and
# related or neighboring rights to this document.
# Attribution is requested but not required.

use strict; use warnings FATAL => 'all';
use v5.10;
use utf8;
# vi:set ai expandtab ts=4:

package Text::Table::Boxed::Pager;

{ no strict 'refs'; ${__PACKAGE__."::VER"."SION"} = 997.999; }
# VERSION from Dist::Zilla::Plugin::OurPkgVersion
# DATE from Dist::Zilla::Plugin::OurDate

require Exporter;
use parent 'Exporter';
our @EXPORT = qw/view_table/;

use Carp;
use Scalar::Util qw/blessed/;

sub get_terminal_columns; #forward

sub view_table($;@) {
  my $tb = shift;
  my %opts = @_;

  unless (blessed($tb) && $tb->isa("Text::Table::Boxed")) {
    croak "Object must be a Text::Table::Boxed\n"
  }

  $opts{lines_per_page} //= get_terminal_rows();

  my $usable_lpp = $opts{lines_per_page} - 1; # leave room for prompt

  my @title_with_rules = $tb->rendered_title();
  my $lines_after_title = $usable_lpp - @title_with_rules;
  my $num_body_rows = $tb->num_body_rows();

  my $rows_viewed_at_quit;
  my $ln_remaining = $usable_lpp;
  my $title_visible = 0;
  TOP:
  for my $rx (0..$num_body_rows-1) {
      my $aref = $tb->rendered_body_rows($rx, 1);
      for my $i (0..$#$aref) {
          if ($i == 0 && $rx != 0) { # start of row other than the first
              if (( # need title first but it's impossible or pointless to show
                    # it on the curent page
                    !$title_visible && @title_with_rules+@$aref > $ln_remaining
                  )
                  ||
                  ( # row won't fit on this page but will fit on a new page
                    @$aref > $ln_remaining && @$aref <= $lines_after_title
                  )
                 )
              {
                  while ($ln_remaining > 0) { print "\n"; --$ln_remaining; } #skip
              }
          }
          if ($ln_remaining==0) {
              print "Press ENTER to continue or q<ENTER> to stop: ";
              STDOUT->flush;
              if (<STDIN> =~ /[qQ]/) {
                $rows_viewed_at_quit = $rx+1;
                last TOP;
              }
              $ln_remaining = $usable_lpp;
              $title_visible = 0;
          }
          if ($i==0 && !$title_visible) { # need title first
              die "bug" if $ln_remaining < @title_with_rules;
              print @title_with_rules;
              $ln_remaining -= @title_with_rules;
              $title_visible = 1;
          }
          die "bug" if $ln_remaining <= 0;
          print $aref->[$i];
          --$ln_remaining;
      }
  }
  return $rows_viewed_at_quit;
}#view_table

#######################################################
# Local copy of Terminalsize.pm (which is not on CPAN)
#######################################################

sub _unix_compatible_os() {
  state $result //=
    # There must be a better way...
    (($^O !~ /win|dos/i && $^O =~ /ix$|ux$|bsd|svr|uni|osf|sv$/)
     || $^O eq 'darwin'
     || $^O eq 'cygwin'
    )
    # This corrupted the special "_" filehandle
    #&& -w "/dev/null" && -x "/bin/echo";
    && (qx'test -w /dev/null && test -x /bin/echo',$?==0)
    ;
  $result;
}

my ($cached_rows, $cached_cols);

sub forget_terminalsize_cache {
  $cached_rows = $cached_cols = undef;
}

sub _get_terminal_size {
  local ($_, $!, $^E);
  my %opts = @_;
  forget_terminalsize_cache() if delete $opts{reset};
  my $debug = delete $opts{debug};
  my $no_defaults = delete $opts{no_defaults};
  croak "unknown option: ",(join " ", keys %opts) if %opts;
  my ($rows, $cols);
  my ($rows_source, $cols_source);

  # If env vars are set, use them but do not cache their values
  if ($ENV{ROWS} && $ENV{COLUMNS}) {
    $rows = $ENV{ROWS};
    $cols = $ENV{COLUMNS};
    $rows_source = $cols_source = "ROWS & COLUMNS env vars";
  }
  if ($ENV{ROWS}) {
    $rows = $ENV{ROWS};
    $rows_source = "ROWS env var";
  }
  if ($ENV{COLUMNS}) {
    $cols = $ENV{COLUMNS};
    $cols_source = "COLUMNS env var";
  }
  if (!$rows && $cached_rows) {
    $rows = $cached_rows; $rows_source = "cached";
  }
  if (!$cols && $cached_cols) {
    $cols = $cached_cols; $cols_source = "cached";
  }
  if (!$rows || !$cols) {
    if (_unix_compatible_os) {
      #if (-t STDERR) {
      {
        if (((
              (-t STDIN) ? qx'stty size 2>/dev/null' :
              (-t STDERR && -r "/dev/fd/2") ? qx'stty -F /dev/fd/2 size' :
              (-w "/dev/tty") ? qx'stty -F /dev/tty size 2>/dev/null' :
              undef
             )//"") =~ /(\d+)\s+(\d+)\s*$/s) {
          if (! $rows) { $rows = 0+$1; $rows_source .= "stty"; }
          if (! $cols) { $cols = 0+$2; $cols_source .= "stty"; }
          ($cached_rows, $cached_cols) = ($1, $2);
        }
        # This used to do qx'/bin/echo -e "lines\ncols" | tput -S 2>/dev/null' =~ /(\d+)\n(\d+)/s
        # but that did not work because that redirects all three std* files and
        # tput does not query /dev/tty without the "init" directive.
        # https://askubuntu.com/questions/1503886/tput-does-not-try-dev-tty/1503889#1503889
        #
        elsif (qx'{ tput lines; tput cols; } 2>/dev/null' =~ /\A(\d+)\s*\R(\d+)\s*\z/s) {
          if (! $rows) { $rows = 0+$1; $rows_source .= "tput"; }
          if (! $cols) { $cols = 0+$2; $cols_source .= "tput"; }
          ($cached_rows, $cached_cols) = ($1, $2);
        }
        else {
          unless ($no_defaults) {
            if (! $rows) { $rows=24; $rows_source .= "hard-coded default"; }
            if (! $cols) { $cols=80; $cols_source .= "hard-coded default"; }
          }
          # The no_defaults option was intended to allow the caller to detect
          # if there is no controlling terminal by checking for undef results.
          # However this does not work because 'tput' returns hard-coded
          # defaults in that case so we never know.
        }
      }
    }
    else {
      die "(fixme) Unrecognized OS $^O or no /dev/null"
    }
  }
  carp "Detected terminal size ($rows,$cols) from ",
       $rows_source eq $cols_source ? $rows_source
                                    : "($rows_source, $cols_source)", "\n"
    if $debug;
  # Forget cached values if either env var is set
  $cached_rows = $cached_cols = undef if $ENV{ROWS} or $ENV{COLUMNS};

  ($rows, $cols)
}

sub get_terminal_rows    { (_get_terminal_size(@_))[0] }
sub get_terminal_columns { (_get_terminal_size(@_))[1] }

=pod

=encoding UTF-8

=head1 NAME

Text::Table::Boxed::Pager - Display table on terminal ala 'more'

=head1 SYNOPSIS

 use Text::Table::Boxed;
 use Text::Table::Boxed::Pager qw/view_table/;

 my $tb = Text::Table::Boxed->new({
   columns => [ ... ],
   style   => "boxrule",
 });
 $tb->load( ... );

 $rows_before_quit = view_table($tb);

 $rows_before_quit = view_table($tb, $num_terminal_rows);

=head1 DESCRIPTION

This displays a table on the terminal, pausing after each screen-full
for the user to press ENTER (or q to stop).

The titles are re-displayed on every screen so they are always visible.

Multi-line rows are kept together on the same screen if possible.

If the size of the terminal is not specified, it is obtained from the OS.

This is basically a demo of using the "rows" feature of L<Text::Table::Boxed>.
See the code.

=head1 RETURN VALUE

Undef is returned if the user paged through the entire table,
otherwise
the number of rows which were displayed before the user
entered 'q' to quit.

=head1 AUTHOR

Jim Avera (jim.avera at gmail)

=head1 LICENSE

CC0 or Public Domain.
However your application is likely subject to the more restrictive licenses
of Text::Table and other modules.

=cut

1;
