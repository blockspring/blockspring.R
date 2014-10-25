# blockspring.R

R library to assist in creating and running blocks (cloud functions) with Blockspring.

### Installation

```R
install.packages("devtools")
library("devtools")
install_github("blockspring/blockspring.R")
```

### Example Usage

Save the following script to an example.R file:
```R
library("blockspring")

myBlock <- function(request, response){
	sum <- as.numeric(request$params$num1) + as.numeric(request$params$num2)
	
	response$addOutput("sum", sum)

	response$end()
}

blockspringDefine(myBlock)
```

Then in your command line write:
```shell
Rscript example.R --num1=20 --num2=50
```

or

```shell
echo '{"num1":20, "num2": 50}' | Rscript example.R
```

### License

MIT

### Contact

Email us: founders@blockspring.com
