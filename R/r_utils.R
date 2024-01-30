# utils.R
# 0.0.1
# 2024-01-25
# D.Kierans (dylanki@kth.se)

#' unlock_envs
#' @description Unlock package- and name-spaces to change function definitions in packages. 
#'  Counterfunction for `lock_envs`
#' @param package_name String - name of package
unlock_envs <- function(package_name){
            # namespace unlock
            env <- asNamespace(package_name)
            rlang::env_unlock(env = env)
            rlang::env_binding_unlock(env = env) 

            # packagespace unlock
            env <- as.environment(paste0("package:",package_name) )
            rlang::env_unlock(env = env)
            rlang::env_binding_unlock(env = env) 
}  

#' lock_envs
#' @description Lock package- and name-spaces to change function definitions in packages
#'  Counterfunction for `unlock_envs`
#' @param package_name String - name of package
lock_envs <- function(package_name){
            # namespace lock
            env <- asNamespace(package_name)
            rlang::env_binding_lock(env = env)
            rlang::env_lock(env)

            # spackspace lock
            env <- as.environment(paste0("package:",package_name) )
            rlang::env_binding_lock(env = env)
            rlang::env_lock(env)
}

#' rTrace_time
#' @description Basic function timer, using R's Sys.time() for now
#' @return Current time
rTrace_time <- function() {
    Sys.time()
}

#' get_env
#' @description Get environment object for this package rTrace
#' @return Environment
get_env <- function() {
   as.environment("package:rTrace") 
}


