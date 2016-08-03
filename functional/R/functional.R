require("gtools")

#' Function composition
#'
#' @param f A function with arity 1
#' @param g A function with arity 1
#' @return A function with arity 1 resulting from composing f and g.
#' @details \%o\% :: (a -> b) -> (c -> a) -> (c -> b)
#' @export
'%o%'  <- function(f, g) {
	return(function(x) { return(f(g(x))) })
}

#' Funcion application
#'
#' @param f A function with arity 1
#' @param v The argunment to F
#' @details This relieves us from the burden of using ')' at the end.
#' @return The result of applying f to v
#' @export
'%$%' <- defmacro(f, v, expr= (f(v)))

#' Apply a function to a dataframe's or matrix columns
#'
#' @param f The function to apply to each column of d.
#' @param d The dataframe or matrix.
#' @return The result of applying f to the columns of d.
#' @export
'%cmap%' <- function(f, d) { return(apply(d,2,f)) }
cmap <- function(f, d) { return(apply(d,2,f)) }


#' select according to a predicate
#'
#' @param x The collections of elements.
#' @param p (a -> Bool) The predicate that the returned elements should verify.
#' @return The subset of elements of x that verify p
#' @export
'%select%'  <- function(x,p){x[p(x)]}

#' generate a function that selects according to a predicate
#'
#' @param p (a -> Bool) a predicate.
#' @return A function of arity 1 that takes a collection and returns that collection's subset of elements that verify p
#' @seealso \%select\%
#' @export

selectBy  <- function(p) { return(function(x) { x %select% p }) }

#' (&&&) :: (b -> c) -> (b -> c') -> (b -> (c, c'))
#'
#' @param f The function whos return value is the first element of the resulting pair.
#' @param g The function whos return value is the second element of the resulting pair.
#' @details This is the function instance's &&& of Arrow.
#' @references Haskell's Control.Arrow
#' @return A function of arity one that returns a pair. r(x) = (f(x), g(x))
#' @export

'%&&&%' <- function(f, g) {
	return(function(arg) { list(fst= f(arg), snd=g(arg)) })
}

#' (***) :: (a -> c) -> (a' -> c') -> ((a, a') -> (c, c'))
#'
#' @param f The function that is applyied to the first pair component
#' @param g The function that is applyied to the second pair component
#' @details This is the function instance's *** of Arrow.
#' @references Haskell's Control.Arrow
#' @return A function of arity one, whith an argument that must be a pair, that returns a pair. r((a,b)) = (f(a), g(b))
#' @export
'%***%' <- function(f, g) {
	return(function(arg) { list(fst= f(arg$fst), snd=g(arg$snd)) })
}

#' Generates function that takes a pair as argument
#'
#' @param f A function of arity 2.
#' @return A function that takes a pair as argument
#' @export
uncurry <- function(f) { return(function(pair) { return(f(pair$fst, pair$snd)) }) }

#' convinient functional version of <=
#'
#' @param i the rhs argument to <=
#' @return a function of arity 1 that returns the result of comparing its argument with i
#' @export
LTE <- function(i) { function(x) { x<=i} }

#' convinient functional version of !
#'
#' @param i the argument to !
#' @return !i
#' @export
NOT <- function(i) { !i }

#' convinient functional version of >
#'
#' @param i the rhs argument to >
#' @return a function of arity 1 that returns the result of comparing its argument with i
#' @export
GT <- function(i) { NOT %o% LTE(i) }

#' convinient functional version of ==
#'
#' @param i the rhs argument to ==
#' @return a function of arity 1 that retutns the result of comparing its argument with i
#' @export
EQ <- function(i) { return (function(x) { x == i }) }

#' The identity function.
#'
#' @param x the value to be returned.
#' @return x.
#' @export
id  <- function(x) { return(x) }

# Lazy persistent sessions


#' creates a constant function
#'
#' @param v the return value of the constant function
#' @details This is sugar for \code{function() { return(v) }}
#' @return \code{function() { return (v) }}
#' @export
const <- defmacro(v, expr= function() { return(v) })

#' creates a lazy value
#'
#' @param v the value that should be computed only when required
#' @details The value is computed only once and reused upon succesive calls.
#' @return a constant function (i.e. a function with no arguments). The first
#' time this function is called the computation v is performed and its value stored
#' in memory to be recalled upon succesive calls.
#' @export
lazy <- defmacro(v, expr= .lazy(function() {return(v)}))

.lazy <- function(e, msg="") {
	done <- FALSE
	value  <- NULL
	return(function() {
		if (!done) {
			if (nchar(msg)>0) {
				print(msg)
			}
			done <<- TRUE
			value <<- e() }
		return(value)
	}) }

.lazyLoad <- function(varname, filepath) {
	done <- FALSE
	value <- NULL
	return(function() {
		if (!done) {
			cat("RESTORING ", varname, "\n")
			done <<-  TRUE
			load(filepath)
			value <<- eval(parse(text=varname))
		}
		return(value)
	})
}

#' persist a valut to storage
#'
#' @usage n \%<-\% value
#' @param n The name where to bind the returned function
#' @param value The value to compute.
#' @return A constant function. The first time this function is called, it will
#' try to read the persisted value from disk. If the file is not present, it
#' will compute and persist it.
#' @export
'%<-%' <- defmacro(n, value, expr = n <- .persistent(n, function(){value}))

persistedLocation <- "tmp-functions-r-package"

if (!(dir.exists(persistedLocation))) {
	dir.create(persistedLocation)	
}

.persistent <- function(varname, e) {
	varname <- as.character(substitute(varname))
	filepath <- paste(persistedLocation, varname, ".RData", sep="/")
	if (file.exists(filepath)) {
		return(.lazyLoad(varname, filepath))
	} else {
		return(.lazy(function() {
			cat("PERSISTING :", varname, "...")
			env <- parent.frame()
			env[[varname]] <- e()
			eval(parse(text=paste("save(",varname,", file=filepath, envir=env)")))
			cat("DONE\n")
			return(env[[varname]]) }))
	}
}
