# https://topepo.github.io/caret/using-your-own-model-in-train.html
# https://github.com/topepo/caret/tree/master/models/files

#' @name mclustda
#' @aliases mclustda
#' 
#' @title Code to support caret interface for Gaussian mixtures in
#' classification from mclust package
#' 
#' @description A list of elements used by \code{\link[caret]{train}}
#' for training and hyper-parameter tuning of Gaussian mixture models 
#' in classification fitted by \code{\link[mclust]{MclustDA}}.
#' 
#' @examples
#' str(mclustda, give.attr = FALSE)
#' \dontrun{
#' mod1 = train(x = iris[,1:4], y = iris$Species, 
#'             method = mclustda, 
#'             trControl = trainControl(method = "cv", number = 10))
#' mod1
#' plot(mod1)
#' 
#' mod2 = train(x = iris[,1:4], y = iris$Species, 
#'              method = mclustda, 
#'              metric = "BrierScore", maximize = FALSE,
#'              trControl = trainControl(method = "cv", number = 10,
#'                                       summaryFunction = mclassSummary,
#'                                       classProbs = TRUE))
#' mod2
#' plot(mod2)
#' 
#' mod2$results$G = as.factor(mod2$results$G)
#' ggplot(mod2$results, aes(y = .data[[mod2$metric]],
#'                         x = modelName, 
#'                         col = G, 
#'                         shape = modelType)) +
#'   geom_point()
#' }
#' @importFrom mclust MclustDA predict.MclustDA
#' @export

mclustda <- list(
  label = "Gaussian mixtures classification",
  library = "mclust",
  type = "Classification",
  # tuning parameters
  parameters = data.frame(parameter = c("G",
                                        "modelName",
                                        "modelType"),
                          class = c("numeric",
                                    "character",
                                    "character"),
                          label = c("Number of mixture components",
                                    "Parsimonious covariance decomposition",
                                    "Model type")),
  # init tuning param values
  grid = function(x, y, len = NULL, search = "grid") 
  {
    G <- 1:3
    modelType <- c("EDDA", "MclustDA")
    modelNames <- mclust::mclust.options("emModelNames")
    if(search == "grid") 
    {
      grid <- expand.grid(G = G,
                          modelName = modelNames,
                          modelType = modelType)
    }
    else {
      # search == "random"
      grid <- data.frame(modelType = sample(modelType, size = len, replace = TRUE),
                         G = sample(G, size = len, replace = TRUE),
                         modelName = sample(modelNames, size = len, replace = TRUE))
    }
    grid <- grid[-which(grid$modelType == "EDDA" & grid$G > 1),]
    rownames(grid) <- 1:nrow(grid)
    return(grid)
  },
  # loop = function(grid) 
  # {
  #   grid <- grid[order(grid$threshold, decreasing = TRUE),, drop = FALSE]
  #   loop <- grid[1,,drop = FALSE]
  #   submodels <- list(grid[-1,,drop = FALSE])       
  #   list(loop = loop, submodels = submodels)
  # },
  # fitting model
  # @param x, y: the current data used to fit the model
  # @param wts: optional instance weights (not applicable for this particular model)
  # @param param: the current tuning parameter values
  # @param lev: the class levels of the outcome (or NULL in regression)
  # @param last: a logical for whether the current fit is the final fit
  # @param weights: weights
  # @param classProbs: a logical for whether class probabilities should be computed
  fit = function(x, y, wts, param, lev, last, weights, classProbs, ...) 
  {
    mclust::MclustDA(data = x, class = y, 
                     modelType = as.character(param$modelType),
                     modelNames = as.character(param$modelName),
                     G = param$G,
                     verbose = FALSE, ...)
  },
  # class predictions
  # @param modelFit: the model produced by the fit code
  # @param newdata: the predictor values to be used for predictions 
  # @param preProc: preprocess data option
  # @param submodels: only used with the loop element
  predict = function(modelFit, newdata, preProc = NULL, submodels = NULL) 
  {
    mclust::predict.MclustDA(modelFit, newdata = newdata)$classification
  },
  # class probabilities predictions
  # @param modelFit: the model produced by the fit code shown above
  # @param newdata: the predictor values of the instances being predicted (e.g. out-of-bag samples)
  # @param preProc: preprcess data option
  # @param submodels: only used with the loop element
  prob = function(modelFit, newdata, preProc = NULL, submodels = NULL)
  {
    mclust::predict.MclustDA(modelFit, newdata = newdata)$z
  },
  # variable importance metrics
  # @param modelFit: the model produced by the fit code shown above
  # @param x, y: the current data used to fit the model
  # varImp = function(modelFit, x = NULL, y = NULL, ...) 
  # {
  #   ??
  # }
  levels = function(x) levels(x$class),
  tags = c("Discriminant Analysis", "Mixture Model", "Supports Class Probabilities"),
  sort = function(x) x[order(x[,1]),]
)
