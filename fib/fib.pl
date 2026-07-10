use strict;
use warnings;

sub fib {
    my ($n) = @_;
    if ($n < 2) {
      return $n;
    }  else {
      return fib($n-1) + fib($n-2);
    }
}

print fib(10)
