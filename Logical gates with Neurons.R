#
# Arpit Chaukiyal - AXC176630
# Assignment 3: Neural Network - Mini Project.
#

#Sigmoid activation function.
#The input parameter would be a numeric
#In the function there is a check, if the input parameter is not numeric it will throw the error.
#
sigmoid_act <- function(a){
    if(!is.numeric(a))
        return(print("Input should be a numeric"))
    return(1/ 1+ exp(-a))
}

sigmoid_act(-10)
sigmoid_act(10)

#Binary activation function.
#The input parameter would be a numeric
#In the function there is a check, if the input parameter is not numeric it will throw the error.
#
binary_act <- function(a){
    if(!is.numeric(a))
        return(print("Input should be a numeric"))
    return(ifelse(a < 0,0,1))
}

binary_act(-10)
binary_act(0)
binary_act(10)

#Dipicting the output of the nueron.
# Arguments:
# b : a numeric value, this arguments hold the Biase of the neuron.
# w : a numeric vector, this arguments holds the vector containing the weights of the input edges.
# x : a numeric vector, this arguments holds the vector containing the input values to the neuron.
#
neuron_in <- function(b, w, x){
    # exception handling of the inputs
    if(!is.numeric(b))
        return(print("The biased should be numeric"))
    if(!is.numeric(w))
        return(print("The weights should be numeric"))
    if(!is.numeric(x))
        return(print("The input should be numeric"))
    if(length(w)!= length(x))
        return(print("The length of the Weight vector and Input vector should be same"))
    out <- 0
    sum <- 0
    for(i in 1:length(x))
    {
        out <-  w[i]*x[i]
        sum <- sum + out
    }
    return(b+sum)
}

neuron_in(-10, c(20,20),c(1,1))

# Neuron with binary step function.
#
neuron_binary <- function(b,w,x){
    return(binary_act(neuron_in(b,w,x)))
}

neuron_binary(-10, c(20,20),c(1,1))

# Neuron with sigmoid step function.
#
neuron_sigmoid <- function(b,w,x){
    return(sigmoid_act(neuron_in(b,w,x)))
}

neuron_sigmoid(-10, c(20,20),c(1,1))

# A generic neuron fuction for both sigmoid or step step functions
# depending upon the value of "func" the neuron will automatically adapts
# the activation function and returns the calculated value of its output.

neuron <- function(b,w,x,func){
    return(func(neuron_in(b,w,x)))
}


# An implementation of the xor logic with the neural network. 
# Arguments:
# x : an input numeric vector. 
#
xor_implem <- function(x){
    # As the we are using binary step functio  to impletemt the XOR gate logic
    # binary_act is passed to func_c variable
    func_c <- binary_act
    # As per the diagram mentioned in the question 5, I have hard-coded the weights
    # and the bias of all the three neurons.
    # w1 and b1 are the weights and bias for neuron 'h1' repectively
    w1 <- c(20,20)
    b1 <- -10
    
    # w2 and b2 are the weights and bias for neuron 'h2' repectively
    w2 <- c(-20,-20)
    b2 <- 30
    
    # w_in and b_in are the weights and bias for neuron 'y' repectively
    w_in <- c(20,20)
    b_in <- -30
    
    #printing the ouput of the neural network.
    print(neuron(b_in, w_in, c(neuron(b1, w1, x, func_c),neuron(b2, w2, x, func_c)), func_c))
}

xor_implem(c(1,1))
xor_implem(c(1,0))
xor_implem(c(0,1))
xor_implem(c(0,0))

