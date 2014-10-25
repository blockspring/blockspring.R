# blockspring.R

R library to assist in creating and running blocks (cloud functions) with Blockspring.

### Installation

```R
install.packages("devtools")
library("devtools")
install_github("blockspring/blockspring.R")
```

### Example Usage

```R
library("blockspring")

myBlock <- function(request, response){
	sum <- as.numeric(request$params$num1) + as.numeric(request$params$num2)
	
	response$addOutput("sum", sum)

	response$end()
}

blockspringDefine(myBlock)
```

### License

MIT

### Contact

Email us: founders@blockspring.com
