use strict; use warnings FATAL => 'all';
use v5.10;
use utf8;
# vi:set ai expandtab ts=4:

#########################################################################
# Wrapper for Text::Table which uses Unicode box-drawing characters for
# borders and handles cells with embedded newlines.
#########################################################################
# Based on code in the Text::Table documentation and
# https://stackoverflow.com/questions/30762521/how-can-i-print-a-table-with-multi-line-strings-using-the-texttable-module

package Text::Table::Boxed;
use base 'Text::Table';

use Carp;
use Scalar::Util qw/reftype/;
use List::Util qw/min max/;
use List::MoreUtils qw/true false any none firstidx/;

use overload (
    bool => sub { return 1; }, # Don't stringify just for a boolean test
    '""' => 'rendered_stringify',
);

use Data::Dumper::Interp qw/visnew dvis vis avis u/;

# Re-implement btw & oops instead of requiring Spreadsheet::Edit::Log
sub btw {  # "by the way..."
  my ($fname, $lno) = (caller(0))[1,2];
  my $msg = join "", ($fname =~ s/.*[\/\\]//r), ":", $lno, " ", @_;
  $msg .= "\n" unless $msg =~ /\R\z/;
  warn $msg;
}
sub oops { @_ = ("OOPS:",@_); goto &Carp::confess }

our $debug = 0;

our %builtin_pictures = (

ascii => <<'EOF',
+-----------+
| a | a | a |
+===========+
| a | a | a |
+---+---+---+
| a | a | a |
+-----------+
EOF

boxrule => <<'EOF',
┌───┬───┬───┐
│ a │ a │ a │
╞═══╪═══╪═══╡
│ a │ a │ a │
├───┼───┼───┤
│ a │ a │ a │
└───┴───┴───┘
EOF

outerbox => <<'EOF',
┌───────┐
│ a a a │
│ a a a │
│ a a a │
└───────┘
EOF

);

sub parse_rulepicture($) {
  my @lines;
  if (ref($_[0]) && reftype($_[0]) eq "ARRAY") {
    @lines = @{ $_[0] };
  }
  elsif (! reftype($_[0])) {
    @lines = map{ "$_\n" } split /\R/, $_[0];
  }
  else { croak "Invalid arg" }

  if ($debug) { btw "--- INPUT ---\n", @lines, "---(end)---\n"; }

  my sub _is_sepspec($) { $_[0] =~ /\p{Alpha}/ }
  my sub _is_rule($)    { $_[0] !~ /\p{Alpha}/ }

  my ($top_rule, $aftertitle_rule, $mid_rule, $bot_rule);
  my ($title_sepspec, $body0_sepspec, $bodyN_sepspec);

  if (_is_rule($lines[-1])) {
    $bot_rule = pop @lines;
  }
  if (_is_rule($lines[0])) {
    $top_rule = shift @lines;
  }
  if (@lines == 0) {
    croak "No content-separator spec line(s)" unless @lines;
  }
  if (@lines == 1) {
    $title_sepspec = $body0_sepspec = $bodyN_sepspec = $lines[0];
  }
  elsif (@lines==3) {
    $title_sepspec               = $lines[0];
    $aftertitle_rule          = $lines[1];
    $body0_sepspec = $bodyN_sepspec = $lines[2];
  }
  elsif (@lines==5) {
    $title_sepspec               = $lines[0];
    $aftertitle_rule          = $lines[1];
    $body0_sepspec               = $lines[2];
    $mid_rule                 = $lines[3];
    $bodyN_sepspec               = $lines[4];
  }
  else {
    croak "Invalid rulepicture\n","\@lines=",scalar(@lines);
  }

  my ($rule_len, $other_rule);
  foreach ($top_rule, $aftertitle_rule, $mid_rule, $bot_rule) {
    next unless defined;
    croak "Expecting a rule line, not ",u($_),"\n" unless _is_rule($_);
    croak "Empty rule line$\n" if length($_)==0;
    if (defined $rule_len) {
      croak "Rule lines must be same length\n(not ",vis($other_rule)," and ",vis($_),")\n"
        if length($_) != $rule_len;
    } else {
      $rule_len = length($_);
      $other_rule = $_;
    }
  }
  foreach ($title_sepspec, $body0_sepspec, $bodyN_sepspec) {
    croak "Expecting a content-separator spec line, not ",u($_),"\n"
      if defined($_) && !_is_sepspec($_);
  }

  if ($debug) {
    foreach my $name (qw/top_rule aftertitle_rule mid_rule bot_rule
                         title_sepspec body0_sepspec bodyN_sepspec/) {
      no strict 'refs';
      my $val = eval "\$$name"; oops $@ if $@;
      btw sprintf "%-15s = %s\n", $name, vis($val);
    }
  }

  foreach ($title_sepspec, $body0_sepspec, $bodyN_sepspec) {
    chomp;
    # "a │ a │ a"     --> ["",   " │ ", " │ ", ""  ]
    # "| a │ a │ a |" --> ["| ", " │ ", " │ ", " |"]
    $_ = [ split /\p{Alpha}/, $_,-1 ];
  }
  btw dvis '$bodyN_sepspec' if $debug;
  unless ("@$body0_sepspec" eq "@$bodyN_sepspec") {
    # These both exist in a picture just to distinguish a mid-rule, if present.
    # Text::Table does not support different horizontal separators for
    # different body rows (only the title row may have different separators).
    croak "All body rows must use the same separators\n";
  }
  # RULE GENERATORS
  # Each rule generator uses a pair of callbacks, herein called the "field"
  # and "separator" callbacks.  Each provides characters to put into a rule
  # line which will be under/over a field or separator string, respectively.
  #
  # The callback arguments are ($index, $num_chars)
  #
  # For the "field callback", $index counts *fields*.
  #
  # For the "separator callback", $index is intended to count *characters* in
  # the concatnation of all separator strings, but **IS UNPREDICTABLE**
  # in some cases due to a bug in Text::Table
  #   See https://github.com/shlomif/Text-Table/issues/13
  #
  # To work around this, state variables are used to count characters ourself.
  # This assumes callbacks occur in left-to-right order (verified by looking
  # at the code).  The state variables are reset whenever $index==0 is seen.
  #
  foreach ($top_rule, $aftertitle_rule, $mid_rule, $bot_rule) {
    next unless defined;
    chomp;
    my $rulestr = $_;

    # Consider this definition with a strange toprule:   <%#%%++-***.&&.>
    # The rule charaters over/under FIELDS are           │ t  ║ t │ t   │
    # '#', '-', and '.'.  We need to save a list of just the single
    # characters from the rule-spec corresponding to the field-data
    # stand-in letters.  The $index callback parameter will select one.
    #
    # The other characters are under/over SEPARATORS.  In this example the
    # separator sub strings are         "│ ", "  ║ ", " │ ", and "   │" and the
    # corresponding rule characters are "<%", "%%++", "***", and "&&.>".
    # We need to save the concatnation of those corresponding characters,
    # which can be indexed using $index passed to the callback.

    # All separator specification must have identically-sized separators,
    # so it does not matter which one we look at:
    #
    my $concat_seprule_chars = "";
    my @field_rule_chars; # array of single characters
    my $rule_ix = 0;
    pos($rulestr) = 0;
    foreach (@$bodyN_sepspec) {
      my $sep_len = length($_);
      $concat_seprule_chars .= substr($rulestr, $rule_ix, $sep_len);
      $rule_ix += $sep_len;
      $rulestr =~ /\G.{$sep_len}(.?)/g or oops dvis '$rulestr $sep_len';
      push @field_rule_chars, $1;
      $rule_ix += length($1); # 1 except 0 at the very end
    }
btw dvis 'BBB @field_rule_chars $concat_seprule_chars pos(rulestr)=',u(pos($rulestr)) if $debug;

    my $field_callback = sub {
        my ($index, $len) = @_;
        my $char = $field_rule_chars[$index] // oops;
        warn "-fld- [$index] len=$len returning '$char'\n" if $debug;
        return ( $char x $len );
    };
    my $separator_callback = sub {
        my ($index, $len) = @_;
        state $char_ix;
        $char_ix = 0 if $index==0;
        my $str = substr($concat_seprule_chars, $char_ix, $len);
        $char_ix += $len;
        warn "=SEP= [$index] len=$len returning '$str'\n" if $debug;
        return $str;
    };
    # replace the rule with the pair of subrefs
    $_ = [ $field_callback, $separator_callback ];
  }
  return {
    top_rule => $top_rule,
    aftertitle_rule => $aftertitle_rule,
    mid_rule => $mid_rule,
    bot_rule => $bot_rule,
    title_sepspec => $title_sepspec,
    # body0_sepspec => $body0_sepspec,  # not supported by Text::Table
    # bodyN_sepspec => $bodyN_sepspec,  # not supported by Text::Table
  }
}#parse_rulepicture

sub insert_seps($$) {
  my ($hash, $columns) = @_;
  my $specs = $hash->{title_sepspec} // oops;
  my @withseps = (
    map{ ( \($_ < $#$specs-1 ? $specs->[$_] : $specs->[-2]), $columns->[$_] ) }
       0..$#$columns
  );
  push @withseps, \$specs->[-1];
  return @withseps;
}

use constant MYKEY => "_key_".__PACKAGE__;
sub new {
  my $class = shift;
  my %opts = (@_==1 && ref($_[0]) eq "HASH" && exists($_[0]->{columns}))
      ? shift(@_)            # new API
      : ( columns => [@_] ); # old API

  croak "'columns' must be provided in OPTIONS\n"
    unless defined ($opts{columns});
  croak "'columns' must be an array ref\n"
    unless reftype($opts{columns}) eq "ARRAY";

  $opts{picture} //= $builtin_pictures{ascii};  # the default

  if (defined $opts{style}) {
      $opts{picture} = $builtin_pictures{ $opts{style} }
        // croak "Invalid 'style' \"$opts{style}\"\n";
  }

  my $hash = parse_rulepicture($opts{picture});
  my @titles_with_separators = insert_seps($hash, $opts{columns});
  my $self = Text::Table::new(__PACKAGE__, @titles_with_separators);

  # Now actually render the rules using the generated callbacks
  foreach (qw/top_rule aftertitle_rule mid_rule bot_rule/) {
    # This anticipates multiple-midrule support in the future
    my $gen = $hash->{$_};
    #next unless defined($gen);
    my @generators = ref($gen->[0]) eq "ARRAY" ? @$gen : ($gen);
    my @rendered_rules = map{
      my ($field_callback, $separator_callback) = @$_;
      $self->rule($field_callback, $separator_callback);
    } @generators;
    $opts{$_} = @rendered_rules > 1 ? \@rendered_rules : $rendered_rules[0];
  }

  $opts{row_starts} = [ 0 ];
  $opts{next_rx} = $self->title_height;

  $self->{MYKEY} = \%opts;
  $self
}#new

sub top_rule { $_[0]->{MYKEY}->{top_rule} }
sub aftertitle_rule { $_[0]->{MYKEY}->{top_rule} }
sub mid_rule {
    for ($_[0]->{MYKEY}->{mid_rule}) {
       my @rules = ref($_) eq "ARRAY" ? @$_ : ($_);
       if (wantarray) { return @rules }
       croak "mid_rule called in scalar context but multiple were defined"
         if @rules > 1;
       return $rules[0];
    }
}
sub bottom_rule { $_[0]->{MYKEY}->{bot_rule} }

sub num_rows { scalar @{ $_[0]->{MYKEY}->{row_starts} } }

sub num_body_rows { $_[0]->num_rows() - 1 }

sub rendered_table_height {
    my $self = shift;
    return(
        1                          # the top_rule
      + $self->table_height()      # data lines
      + $self->num_rows            # a rule line at the end of each row
    );
}

sub add {
    my $self = shift;
    my $opts = $self->{MYKEY};

    # Calculate height of the row, taking into account embedded newlines
    # (which Text::Table will split, inserting multiple lines)
    my $height = 1;
    foreach (@_) {
      $height += scalar(@{[ /\R/g ]});
    }
    push @{ $opts->{row_starts} }, $self->table_height;

    $self->SUPER::add(@_);
}

sub rendered_title {
    my $self = shift;
    return ($self->{top_rule},
            $self->title(),
            $self->{aftertitle_rule}
           )
}

sub rows {
    my ($self, $row_index, $num_rows, $_with_rules) = @_;
    $row_index //= 0;
    $num_rows //= 1;
    my $opts = $self->{MYKEY};
    my $row_starts = $opts->{row_starts};
    croak "Negative index not supported\n" if $row_index < 0 or $num_rows < 0;
    my $max_rx = $#{ $row_starts };
    croak "Row index out of range\n"
      if $row_index+$num_rows-1 > $max_rx;

    my @results = map{
       my $first_lx = $row_starts->[$_];
       my $nextrow_first_lx =
           ($_ == $max_rx ? $self->height() : $row_starts->[$_+1]);
       my $num_lines = $nextrow_first_lx - $first_lx;

       [
         (($_with_rules && $_ == 0) ? $opts->{top_rule} : ()),

         $self->table($first_lx, $num_lines),

         ($_with_rules ?
           ($_ == 0        ? $opts->{aftertitle_rule} :
            $_ == $max_rx ? $opts->{bot_rule} :
                             $opts->{mid_rule}
           ) : ()
         )
       ]
    } $row_index..$row_index+$num_rows-1;

    if (wantarray) { return @results; }
    croak "Scalar context but multiple rows in results\n"
      if @results > 1;
    return $results[0];
}#rows

sub body_rows {
    my ($self, $row_index, $num_rows, $_with_rules) = @_;
    $self->rows($row_index+1, $num_rows, $_with_rules);
}
sub rendered_body_rows {
    my ($self, $row_index, $num_rows) = @_;
    return $self->body_rows($row_index, $num_rows, 1);
}

sub title_row {
    my ($self, $_with_rules) = @_;
    return $self->body_rows(0, 1, $_with_rules);
}
sub rendered_title_row {
    my $self = shift;
    return $self->title_row(1);
}

sub rendered_title_height() { $_[0]->title_height() + 2 }

sub rendered_rows {
    my ($self, $row_index, $num_rows) = @_;
    return $self->rows($row_index, $num_rows, 1);
}

sub rendered_table {
    my ($self, $start_lx, $num_lines) = @_;
    $start_lx //= 0;
    $num_lines //= 1;
    my $opts = $self->{MYKEY};
    my $row_starts = $opts->{row_starts};

    croak "Negative index not supported\n" if $start_lx < 0 or $num_lines < 0;
    my $max_lx = $self->rendered_table_height() - 1;
    my $last_lx = $start_lx + $num_lines;
    croak "Line index $start_lx+$num_lines-1 is out of range (max=$max_lx)\n" 
      if $start_lx+$num_lines-1 > $max_lx;

    # Retrieve rows and flatten result, truncating some lines if appropraite

    my $last_rx = $last_lx >= $row_starts->[-1]
                     ? $#$row_starts
                     : first{ $last_lx >= $row_starts->[$_] }
                       reverse(0..$#$row_starts-1) ;

    my $first_rx = $start_lx >= $row_starts->[$last_rx]
                     ? $last_rx
                     : first{ $start_lx < $row_starts->[$_+1] } 0..$last_rx-1 ;

    my @lines = (
        map{ @$_ } $self->rendered_rows($first_rx, $last_rx-$first_rx+1)
    );
    splice @lines, $last_lx-$start_lx+1; # trunc undesired lines from last row

    return (wantarray ? @lines : join("", @lines));
}

sub rendered_body {
    my ($self, $start_lx, $num_lines) = @_;
    return
        $self->rendered_table( $start_lx + $self->rendered_title_height(),
                               $num_lines );
}

sub rendered_stringify {
    my $self = $_[0];
    return( scalar $self->rendered_table() );
}

=pod

=encoding UTF-8

=head1 NAME

Text::Table::Boxed - Automate separators and rules for Text::Table

=head1 SYNOPSIS

    use Text::Table::Boxed;

    my $tb = Text::Table::Boxed->new({
      columns => [ "Planet", "Radius\nkm", "Density\ng/cm^3" ],
      style   => "boxrule",
    });

    $tb->load(
        [ "Mercury", 2360, 3.7 ],
        [ "Venus", 6110, 5.1 ],
        [ "Earth", 6378, 5.52 ],
        [ "Jupiter", 71030, 1.3 ],
    );

    print $tb;  # Render table including separators and rules

    # Custom rules and separators
    my $tb = Text::Table::Boxed->new({
      columns => [ "Planet", "Radius\nkm", "Density\ng/cm^3" ],
      picture => [<<'EOF'],
    ┌───╥───┬───┐
    │ a ║ a │ a │
    ╞═══╬═══╪═══╡
    │ a ║ a │ a │
    ├───╫───┼───┤
    │ a ║ a │ a │
    ╘═══╩═══╧═══╛
    EOF
    );

    # Retrieve the lines in the title, including rules before and after
    my @lines = $tb->rendered_title();

    # Retrieve rows, each of which is an array of possibly-multiple lines,
    # the last of which is a rule line.
    my @linesets = $tb->rendered_body_rows($row_index, $num_rows);

=head1 DESCRIPTION

This wrapper for L<Text::Table> automates column separators
and horizontal rules, taking into account cells containing
embedded newlines lines.

Support for ASCII or Unicode box-drawing characters is built in.
Custom ruling can be specified using an "ascii art" picture of what you want.

B<Text:Table::Boxed> is a derived class and
supports all the methods of Text::Table.
C<new> supports a different API where a single hashref argument supplies
column descriptors in a 'columns' element, along with possibly other options.

=head1 ROWS

The concept of "B<row>" is introduced, which represents a possibly-multiline
table row.  Rules are inserted only between I<rows>, not between lines
within a multi-line row.  Multi-line rows exist where cell values
contain embedded newlines.

The height of a row is the number of lines in the cell with the most
embedded newlines.

Methods with "B<row>" in their name return lines segregated into rows, each
row represented by an array of the constituent lines.
In contrast, similar methods without "row" in their name return a flat
list of lines (or in scalar context, a single possibly-multiline string).

Methods with "B<rendered>" in their name include I<rule lines> in their
result.  For example B<rendered_table()> returns all lines in the table
including rule lines, whereas B<table()> omits rule lines.

I<Rules> are inserted when B<rendered_*()> methods are called, however
I<separators>, i.e. characters between columns and at the edges,
are inserted into the column descriptor list by 'new' and so exist in the
underlying L<Text::Table> object.  Consequently methods like B<table()>
include horizontal separator characters (but not rule lines), and the
B<width()> returns the number of characters in each line including
separators.

=head1 OPTIONS

=over 4

=item B<columns> => [ column titles... ]

A list of column titles or descriptor hashrefs, as documented in L<Text::Table>.

Separators should not be included here.

=item B<style> => "ascii" | "boxrule"

Use a built-in set of separator and rule characters.

=item B<picture> => [ lines ];

Specify separators and rule lines using a picture of what you want
(see example in the SYNOPSIS).

"Rule" lines contain exactly what should be displayed for a table
the same size as the picture.  Characters are replicated as needed to fit
the actual column widths.

"Data" lines use any single alphabetic letter as a stand-in for cell content;
other characters are taken as separators including any spaces (e.g. for
padding).  Separators are inserted between the columns and at the edges.

The picture should have two or more columns so that it has between-cell
separators.  If more columns are included, different separators may be
before each columns (the last between-col separator in the picture
will be used for additional columns in the real table).

Four rules may be specified:

=over 4

=item * Top rule (optional)

=item * After-title rule (if omitted, the Mid rule is used)

=item * Mid rule, used between body rows

=item * Bottom rule (optional)

=back

See "PICTURE SPECIFICATIONS" for more.

=back

=head1 ADDITIONAL PUBLIC METHODS

=head2 Status Information

=over 4

=item num_body_rows()

The number of possibly-multiline I<rows> in the body portion of the table.  See B<ROWS> above.

    $numrows = $tb->num_body_rows;

=item rendered_title_height()

The number of lines in the title I<row>, including rules before and after
(unlike B<title_height()> which only counts non-rule lines).

    $numlines = $tb->rendered_title_height;

=back

=head2 Table Output

=over 4

=item body_rows()

=item rendered_body_rows()

Returns I<rows>, each of which is a ref to an array containing
possibly-multiple lines.  In scalar context, returns a single array ref
(valid only for a single row).
B<rendered_body_rows()> includes a rule line as the last line in each row.

Row index 0 is the first body row.

    @linesets = $tb->rendered_body_rows;
    @linesets = $tb->rendered_body_rows($row_index, $num_rows);
    $lineset  = $tb->rendered_body_rows($row_index);

=item title_row()

=item rendered_title_row()

Returns a row representation (i.e. array ref) for the title row.
The title row will contain multiple lines if title cell values had
embedded newlines.

  $lineset = $tb->title_row
  $lineset = $tb->rendered_title_row

=item rendered_table()

Like C<table()> but includes rule lines.

    @lines = $tb->rendered_table;
    @lines = $tb->rendered_table($line_index, $num_lines);
    $line  = $tb->rendered_table($line_index, $num_lines);

Line index 0 is the initial rule line,
index 1 is the first "real" title line (if there is a title), etc.

=item rendered_stringify()

Returns the entire table as a single string including rule lines.
This is the same as B<rendered_table()> but returns a string even in
array context.

The object also stringifies to the same result using operator overloading.

    $string = $tb->rendererd_stringify()
    $string = $tb;  # same result

=item rendered_title()

Like C<rendered_table()> but only returns lines from the title row,
ending with the rule line following the title.

=item rendered_body()

Returns lines from the body area of the table, including rule lines
after each row.  Row index 0 is the first line of the first body row.

=item top_rule()

=item aftertitle_rule()

=item mid_rule()

=item bottom_rule()

These return the corresponding rendered rule lines.   You normally do not
call these yourself because the rules are automatically included by
the "rendered_xxx" methods

=back

=head1 PICTURE SPECIFICATIONS

Custom separators and rules are specified as a "picture" built from several
lines.  Here are examples:

    ┌─╥─┬─┐   ┌───╥───┬───┐   =============  ⇦  top rule
    │t║t│t│   │ t ║ t │ t │   | a | a | a |
    ╞═╬═╪═╡   ╞═══╬═══╪═══╡   |===+===+===|  ⇦  after-title rule
    │a║a│a│   │ b ║ c │ d │   | b | b | b |
    ├─╫─┼─┤   ├───╫───┼───┤   |---+---+---|  ⇦  mid rule (used only when
    │a║a│a│   │ e ║ f │ g │   | z | z | z |       another body row follows)
    ╘═╩═╧═╛   ╘═══╩═══╧═══╛   =============  ⇦  bottom rule

Single alphabetic letters are stand-ins for real content. Everything
between those letters or at the edge is a separator string.
In the first example, the column separators are single characters, so there
is no padding around content.  In the others the separators are two or three
characters each including spaces for padding, e.g. "|␠", "␠|␠" or "␠|".

Outer borders can be omitted:

    t║t│t    t ║ t │ t     t | t | t
    ═╬═╪═   ═══╬═══╪═══   ===+===+===   ⇦  after-title rule
    a║a│a    b ║ b │ b     b | b | b
    ─╫─┼─   ───╫───┼───   ---+---+---   ⇦  mid rule
    a║a│a    z ║ z │ z     z | z | z       (if another body row follows)

To get only outer borders, omit the interior rules, and use interior
separators containing only spaces (unless you want cell contents to touch):

    ┌─────┐   ┌─────────┐   ===========
    │t t t│   │ t  t  t │    t | t | t
    │a a a│   │ b  b  b │    b | b | b
    │a a a│   │ z  z  z │    z | z | z
    ╘═════╛   ╘═════════╛   ===========

B<RENDERING DEGENERATE DATA:>

If there is only a single 'body' row, the table is rendered with the
bottom rule immediately following the body row:

    ┌──────╥─────┬────────────┐ ⇦  top rule
    │ NAME ║ AC  │  NUMBER    │
    ╞══════╬═════╪════════════╡ ⇦  after-title rule
    │ Sam  ║ 800 │  555-1212  │
    ╘══════╩═════╧════════════╛ ⇦  bottom rule

If there are no 'body' rows, the after-title rule is omitted:

    ┌──────╥─────┬────────────┐ ⇦  top rule
    │ NAME ║ AC  │  NUMBER    │
    ╘══════╩═════╧════════════╛ ⇦  bottom rule

If there is no title, the top rule immediately precedes the first body row:

    ┌──────╥─────┬────────────┐  ⇦  top rule
    │ Sam  ║ 800 │  555-1212  │
    ├──────╫─────┼────────────┤  ⇦  mid rule
    │ Mary ║ 707 │  123-4567  │
    ╘══════╩═════╧════════════╛  ⇦  bottom rule

A completely-empty table renders as only the top and bottom rules:

    ┌──────╥─────┬────────────┐
    ╘══════╩═════╧════════════╛

=head1 PAGER EXAMPLE

The following pages through a table on a terminal one screenful at a time, ala L<more(1)>.
The title is repeated at the top of each page, and multi-line rows are kept together on the
same page if possible:

  my $lines_per_page = get_terminal_rows() - 1; # not counting prompt line
  my @title_with_rules = $tb->rendered_title();
  my $lines_after_title = $lines_per_page - @title_with_rules;
  my $num_rows = $tb->num_body_rows();

  my $ln_remaining = $lines_per_page;
  my $title_visible = 0;
  TOP:
  for my $rx (0..$num_rows-1) {
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
              last TOP if <STDIN> =~ /[qQ]/;
              $ln_remaining = $lines_per_page;
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
  print "Done.\n";

=head1 ACKNOWLEDGMENTS

This module was inspired the example code in the Text::Table
documentation by Anno Siegel and/or Shlomi Fish, and
a post at
L<https://stackoverflow.com/questions/30762521/how-can-i-print-a-table-with-multi-line-strings-using-the-texttable-module>
by stackoverflow user "ThisSuitIsBlackNot".

=head1 BUGS

Text::Table::Boxed is new in 2024 and actively maintained.
Please report issues!

=head1 AUTHOR

Jim Avera (jim.avera at gmail)

=head1 LICENSE

CC0 or Public Domain.
However your application is likely subject to the more restrictive licenses
of Text::Table and other modules.

=cut
1;
