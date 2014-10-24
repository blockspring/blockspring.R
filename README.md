# blockspring.R

R library to assist in creating and running blocks (cloud functions) with Blockspring.

### Installation

```R
install.packages("blockspring")
```

### Example Usage

```R
library("blockspring")

myBlock <- function(request, response){
	sum <- as.numeric(request$params$num1) + as.numeric(request$params$num2)
	
	response$addOutput("sum", sum)

	response$end()
}

define(myBlock)
```

### License

MIT

### Contact

Email us: founders@blockspring.com
