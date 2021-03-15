
# Goal of this script:
# Go through all the module folders
# Collect all the symlinks
# Know renv-cache path
# Subtract renv-cache path from the symlinks to go to relative paths
# Unix:
#   Recreate the symlinks directly on unix but relative this time
# Win:
#   Save them to a file that can be used to restore them on Windows
#   Have a function here or in that file that restores them and add it as a custom action to Wix

pastePath <- function(path) { return(paste0(  path, collapse=.Platform$file.sep)     ) }
splitPath <- function(path) 
{ 
  paths <- strsplit(path, .Platform$file.sep)[[1]]; 
  return(paths[paths != ""]) # Remove "empty" dir between // like: "blablac//weird/path"
}

#This returns atwo functions that can be used to convert paths in two direcitons
determineOverlap <- function(targetRoot, sourceRoot)
{
  targetSplit <- splitPath(targetRoot)
  sourceSplit <- splitPath(sourceRoot)
  len         <- min(length(targetSplit), length(sourceSplit))
  overlap     <- 0

  for(idx in seq(len))
    if(sourceSplit[[idx]] == targetSplit[[idx]]) overlap <- idx
    else                                         break

  overlapVec <- targetSplit[seq(overlap)]
  overlap    <- list(
    vec = overlapVec, 
    str = pastePath(overlapVec),
    len = overlap
  )

  # This function returns the path from the target location to the source as seen from target (aka "(../)*" with either from the root to the source added or not depending on logical addRootToSource
  targetToSource <- function(target, addRootToSource)
  {
    targetSplit  <- splitPath(target)
    rootToSrc    <- pastePath(sourceSplit[seq(overlap$len + 1, length(sourceSplit))])
    stepsDown    <- length(targetSplit) - (overlap$len + as.integer(addRootToSource))
    tgtToSrc     <- pastePath(rep("..", stepsDown)  )

    #for debug:
    tgtToSrc     <- paste0(tgtToSrc, "/.")

    if(addRootToSource)
      return(paste0(tgtToSrc, rootToSrc)) #We do not need to add the separator because it is there in tgtToSrc
    return(tgtToSrc)
  }

  #This one returns the path from the root (overlap) to where target is.
  sourceToTarget <- function(target)
  {
    targetSplit  <- splitPath(target)
    srcToTgt     <- pastePath(targetSplit[seq(overlap$len + 1, length(targetSplit))])

    return(srcToTgt)
  }

  return(list(targetToSource=targetToSource, sourceToTarget=sourceToTarget))
}

#Use overlapfunctions as returned by determineOverlap to generate a function to turn target-path from absolute to relative
getRelativityFunction <- function(modulesRoot, renvCache)
{
  #I wanted this code to be more general but then it is too complicated to debug. And has a bug. And renv-cache can be assumed to be right next to Modules anyway...
  # So instead of doing:
  #   modToRenvF <- determineOverlap(modulesRoot, renvCache)
  #   modToRenvS <-  modToRenvF$targetToSource(renvCache, TRUE)
  # We can assume:
  modToRenvS <- "../../renv-cache"
  #print(paste0("modToRenvS: ", modToRenvS))
  
  return(
    function(linkLocation, targetPath)
    {
      linkLocation <- normalizePath(path.expand(linkLocation))
      targetPath   <- normalizePath(path.expand(targetPath))
      #linkToMod    <- determineOverlap(linkLocation, modulesRoot)$targetToSource
      pathToRenv   <- determineOverlap(targetPath,   renvCache)  $sourceToTarget

      #linkToModS   <- linkToMod(linkLocation, FALSE)
      linkToRenvS  <- modToRenvS #pastePath(c(linkToModS, modToRenvS))
      pathToRenvS  <- pathToRenv(targetPath)

      newTarget    <- paste0(linkToRenvS, .Platform$file.sep, pathToRenvS)

      #print(paste0("for link '", linkLocation, "' and target '",targetPath, "'"))
      #print(paste0("- linkToModS '",linkToModS, " modToRenvS: '", modToRenvS, "' pathToRenvS: '", pathToRenvS, "'\n results in newTarget: '", newTarget, "'"))
      
      return(newTarget)
    }
  )
}

# Returns a list of symlinks with target location relative to modulesRoot
collectLinks <- function(modulesRoot, renvCache)
{
  modulesRoot <- normalizePath(path.expand(modulesRoot))
  renvCache   <- normalizePath(path.expand(renvCache))

  print(paste0("modulesRoot: '", modulesRoot, "' and renvCache: '", renvCache, "'"))

  #setwd(modulesRoot)

  #Sometimes a dutch word just works so much better than english, so here `relativeer > relativize`
  relativeer <- getRelativityFunction(modulesRoot, renvCache)
  symlinks   <- data.frame(linkLocation=character(0), linkTarget=character(0), originalTarget=character(0))

  # copy paste from https://stat.ethz.ch/R-manual/R-patched/library/base/html/Sys.readlink.html
  is.symlink <- function(paths) isTRUE(nzchar(Sys.readlink(paths), keepNA=TRUE))

  collectSymlinks <- function(paths)
    for(path in paths)
    {
      if(is.symlink(path))
      {
        symPath  <- Sys.readlink(path)
        if(substring(symPath, 1, 1) != ".") #if starts with dot it is already relative
          symlinks[nrow(symlinks)+1, ] <<- list(linkLocation=path, linkTarget=relativeer(path, symPath), originalTarget=symPath)
      }
      else
      {
        everything  <- list.files(path, recursive=FALSE, include.dirs=TRUE, all.files=FALSE, full.names=TRUE)
        allDirs     <- everything[file.info(everything)$isdir]
        allSymlinks <- allDirs[is.symlink(allDirs)]
        allDirs     <- setdiff(allDirs, allSymlinks)
      
        collectSymlinks(allDirs)
      }
    }

  collectSymlinks(modulesRoot)

  #print("Found symlinks:")
  #print(symlinks)

  return(symlinks)
}

convertAbsoluteSymlinksToRelative <- function(modulesRoot, renvCache)
{
  symlinks <- collectLinks(modulesRoot, renvCache)

  if(length(symlinks$linkLocation) == 0)
    print("No absolute symlinks found, maybe you ran this script already?")
  else
  {
    #remove absolute links
    unlink(symlinks$linkLocation)
    
    wd <- getwd()

    #little helper to make the log output easier to read
    printSizes <- integer(3)
    padToMax <- function(str, idx)
    {
      printSizes[[idx]] <<- max(nchar(str), printSizes[[idx]])
      needThisMany      <-  printSizes[[idx]] - nchar(str)

      return(paste0(str, paste0(rep(" ", needThisMany), collapse="", sep="")))
    }

    #create the new ones
    for(row in seq(nrow(symlinks)))
    {
      linkLoc <- symlinks[row, "linkLocation"]
      setwd(dirname(linkLoc))
      #print(paste0("For link '", padToMax(symlinks[row, "linkLocation"], 1), "' will convert '", padToMax(symlinks[row, "originalTarget"], 2), "' to '", padToMax(symlinks[row, "linkTarget"], 3), "'"))
      file.symlink(from=symlinks[row, "linkTarget"], to=basename(linkLoc))
      
    }

    setwd(wd)
    warnings()
  }
}


#call like: convertAbsoluteSymlinksToRelative("~/Broncode/build-JASP-Desktop_Qt_5_15_2_clang_64bit-Debug/Modules", "~/Broncode/build-JASP-Desktop_Qt_5_15_2_clang_64bit-Debug/renv-cache")