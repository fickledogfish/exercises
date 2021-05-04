use 5.024;
use strict;
use warnings;
# use diagnostics;

# Enable UTF-8 in source and output
use utf8;
use open ':std', ':encoding(UTF-8)';

# Declare the subroutines used in this file
use subs 'search_text_for', 'select_words';

my $filename = 'test.txt';
my $seed = 'smith';

my $source_text = "";

{
    # Unset the input recorder separator, so that reading from the file handle
    # will result in the entire file rather than a single line.
    #
    # (This needs to be done locally, or the changes will propagate the entire
    # program, and this may break things.)
    local $/ = undef;

    # Attempt to open the file with utf-8 encoding; if it fails, kill the
    # program and report the error.
    open my $file, '<', $filename
        or die "Unable to read file \"$filename\": $!";

    # Read from the file handle until an undef happens (in this case, this will
    # go on until the end of the file).
    $source_text = <$file>;

    # We got what we needed from the file, so it can be closed now.
    close $file;
}

# Get rid of all punctuation and newlines in the source text, since all we need
# is a list of words.
$source_text =~ s/\p{Punct}|\n/ /g;

# Make sure everything is in lower case, just to make it easier to match things.
$source_text = lc $source_text;

# Extract the list of words in the source text (everything should be separeted
# by spaces at this point).
my @words = split / +/, $source_text;

# The Diastic algorithm itself.
say join " ", select_words $seed, \@words;

# Search an array of strings for a string that has a given character in a given
# index (relative to each word).
sub search_text_for {
    my ($char, $index, $array_ref) = @_;

    # search array
    foreach (@$array_ref) {
        if(index($_, $char) != -1) {
            return $_;
        }
    }

    # return found result or an empty string
    return "";
}

# Runs the search_text algorithm for each word character in the seed
sub select_words {
    my ($seed, $words_ref) = @_;

    my @poem = ();

    foreach (split //, $seed) {
        state $index = 0;

        # only attempt if the current char in the seed is a word character
        if($_ =~ /\w/) {
            push(@poem, search_text_for $_, $index, $words_ref);
        }

        $index++;
    }

    return map {$_ eq "" ? () : $_} @poem;
}
