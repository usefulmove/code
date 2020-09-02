quadratic <- function( a, b, c ) {
    root.1 <- ( -b + sqrt( b^2 - 4*a*c ) ) / (2 * a)
    root.2 <- ( -b - sqrt( b^2 - 4*a*c ) ) / (2 * a)

    return( c( root.1, root.2 ) )
}

quadratic.c <- function( a, b, c ) {
    root.1 <- ( -b + sqrt(as.complex( b^2 - 4*a*c )) ) / (2 * a)
    root.2 <- ( -b - sqrt(as.complex( b^2 - 4*a*c )) ) / (2 * a)

    return( c( root.1, root.2 ) )
}
