module PrettyPrint (printJSON) where
    import SimpleJSON -- provides JSONValue definitions
    import Prettify -- provides Doc and conversion from JSONValue to Doc functionality
    
    printJSON :: Doc -> IO()
    printJSON = undefined -- TODO 
