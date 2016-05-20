# neuralnets.R
# R 3.2.4

NeuralNetwork = setRefClass(
  "NeuralNetwork",

  fields = list(
    ni = "integer",
    nh = "integer",
    no = "integer",

    inputs = "array",
    ihWeights = "matrix",
    hBiases = "array",

    hiddens = "array",
    hoWeights = "matrix",
    oBiases = "array",
    outputs = "array"
  ),

  methods = list(
    initialize = function(ni, nh, no) {
      .self$ni <- ni
      .self$nh <- nh
      .self$no <- no

      inputs <<- array(0.0, ni)
      ihWeights <<- matrix(0.0, nrow=ni, ncol=nh)
      hBiases <<- array(0.0, nh)

      hiddens <<- array(0.0, nh)
      hoWeights <<- matrix(0.0, nrow=nh, ncol=no)
      oBiases <<- array(0.0, no)
      outputs <<- array(0.0, no)
    }, # initialize()

    setWeights = function(wts) {
      numWts <- (ni * nh) + nh + (nh * no) + no
      if (length(wts) != numWts) {
        stop("FATAL: incorrect number weights")
      }

      wi <- as.integer(1) # weight index
      for (i in 1:ni) {
        for (j in 1:nh) {
          ihWeights[i,j] <<- wts[wi]
          wi <- wi + 1
        }
      }
      for (j in 1:nh) {
        hBiases[j] <<- wts[wi]
        wi <- wi + 1
      }
      for (j in 1:nh) {
        for (k in 1:no) {
          hoWeights[j,k] <<- wts[wi]
          wi <- wi + 1
        }
      }
      for (k in 1:no) {
        oBiases[k] <<- wts[wi]
        wi <- wi + 1
      }
    }, # setWeights()

    computeOutputs = function(xValues) {
      hSums <- array(0.0, nh) # hidden nodes scratch array
      oSums <- array(0.0, no) # output nodes scratch
      
      for (i in 1:ni) {
        inputs[i] <<- xValues[i]
      }
 
      for (j in 1:nh) { # pre-activation hidden node sums
        for (i in 1:ni) {
          hSums[j] <- hSums[j] + (inputs[i] * ihWeights[i,j])
        }
      }

      for (j in 1:nh) { # add bias
        hSums[j] <- hSums[j] + hBiases[j]
      }

      for (j in 1:nh) { # apply activation
        hiddens[j] <<- tanh(hSums[j])
      }

      for (k in 1:no) { # pre-activation output node sums
        for (j in 1:nh) {
          oSums[k] <- oSums[k] + (hiddens[j] * hoWeights[j,k])
        }
      }

      for (k in 1:no) { # add bias
        oSums[k] <- oSums[k] + oBiases[k]
      }

      outputs <<- my_softMax(oSums) # apply activation
      return(outputs)

    }, # computeOutputs() 

    my_softMax = function(arr) {
      n = length(arr)
      result <- array(0.0, n)
      sum <- 0.0
      for (i in 1:n) {
        sum <- sum + exp(arr[i])
      }
      for (i in 1:n) {
        result[i] <- exp(arr[i]) / sum
      }
      return(result) 
    }

  ) # methods
) # class

# -----

cat("\nBegin neural network demo \n")

numInput <- as.integer(3)
numHidden <- as.integer(4)
numOutput <- as.integer(2)
cat("\nSetting numInput = ", numInput, "\n")
cat("Setting numHidden = ", numHidden, "\n")
cat("Setting numOutput = ", numOutput, "\n")

cat("\nCreating neural network \n")
nn <- NeuralNetwork$new(numInput, numHidden, numOutput)

cat("\nSetting input-hidden weights to 0.01 to 0.12 \n")
cat("Setting hidden biases to 0.13 to 0.16 \n")
cat("Setting hidden-output weights to 0.17 to 0.24 \n")
cat("Setting output biases to 0.25 to 0.26 \n")
wts <- seq(0.01, 0.26, by=0.01)
nn$setWeights(wts)

xValues <- c(1.0, 2.0, 3.0)
cat("\nSetting input values: ")
print(xValues)

cat("\nComputing output values \n")
outputs <- nn$computeOutputs(xValues)
cat("\nDone \n")
cat("\nOutput values: ")
print(outputs)

cat("\nEnd demo\n")
