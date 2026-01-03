setClass("BaseGroupV04", 
         slot = 
           c(ome_attributes = "list"))

# validation
.validate_BaseGroupV04 <- function(object, param = 1)
{
  
}
setValidity("BaseGroupV04", .validate_BaseGroupV04)