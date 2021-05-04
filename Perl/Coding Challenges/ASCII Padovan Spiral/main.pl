# ASCII Padovan Spiral
#
# http://codegolf.stackexchange.com/questions/105304/generate-an-ascii-padovan-spiral
#
# The Padovan Sequence
# ====================
#
# Similar to the Fibonacci sequence, it is created by adding previous terms in
# the sequence as follows:
#
#     P(n+1) = P(n-2) + P(n-3)
#
# and the first 3 terms are defined as:
#
#    P(1) = P(2) = P(3) = 1
#
# so the sequence itself is as follows:
#
#     1, 1, 1, 2, 2, 3, 4, 5, 7, 9, 12, 16, 21, 28, 37, 49, 65, 86, 114, ...
#
# The Padovan Spiral
# ==================
#
# Given the Padovan sequence above, the spiral is created by joining triangles
# in a counter- or clockwise motion, where every triangle has the side length of
# its corresponding position in the Padovan sequence.
#
# A triangle of size 1 should be drawn as:
#
#      /\
#     /__\
#
# If asked for 5 triangles, the output should be:
#
#        /\
#       /  \
#      /    \
#     /______\
#     \      /\
#      \    /__\
#       \  /\  /
#        \/__\/
#
# or, for 8:
#
#          __________
#        /\          /\
#       /  \        /  \
#      /    \      /    \
#     /______\    /      \
#     \      /\  /        \
#      \    /__\/          \
#       \  /\  /            \
#        \/__\/______________\
#         \                  /
#          \                /
#           \              /
#            \            /
#             \          /
#              \        /
#               \      /
#                \    /
#                 \  /
#                  \/

use 5.024;
use utf8;
use strict;
use warnings;
# use diagnostics;

say "Hello, Padovan!";

sub draw_triangle {

}
