
toSent <- function(stringVect){
    paste(stringVect, collapse = " ")
}


## A callable function that writes out the contents
# of a vector in human readable form.
show_vector <- function(var_file_name_vec) {
  for(i in 1:length(var_file_name_vec)) { 
    cat(i, var_file_name_vec[i], "\n", sep=" ")
  } 
}

# A callable function that extracts entities 
# of an identified kind from an 
# AnnotatedPlainTextDocument
extractEntities <- function(obj_doc, var_kind_character) {
  var_content_string <- obj_doc$content
  obj_annotations <- annotations(obj_doc)[[1]]
  if(hasArg(var_kind_character)) {
    var_kindFeatures_list <- sapply(obj_annotations$features, `[[`, "kind")
    var_content_string[obj_annotations[var_kindFeatures_list == var_kind_character]]
  } else {
    var_content_string[obj_annotations[obj_annotations$type == "entity"]]
  }
}

# A callable function that checks to see
# if an annotated document has any matching
# entities
checkForEntities <- function(obj_doc, var_kind_character) {
  obj_annotations <- annotations(obj_doc)[[1]]
  var_return_boolean <- TRUE
  if(hasArg(var_kind_character)) {
    var_kindFeatures_list <- sapply(obj_annotations$features, `[[`, "kind")
    var_matchCount_list <- obj_annotations[var_kindFeatures_list == var_kind_character]
    var_matchCountSize_int <- length(var_matchCount_list)
    if (var_matchCountSize_int < 1) {
      var_return_boolean <- FALSE
    }
  } else {
    var_return_boolean <- FALSE
  }
  return(var_return_boolean)
}

## Label entities

makeAnnTex <- function(text){
    var_textBlob_character <- text

                                        # explicitly convert var_textBlog_character to a Java
                                        # string class.  Necessary because the NLP is written
                                        # in java.
    var_textBlob_string <- as.String(var_textBlob_character)
    
                                        # create the annotators.
    obj_sentence_annotator <- Maxent_Sent_Token_Annotator()
    obj_word_annotator <- Maxent_Word_Token_Annotator()
    obj_entity_annotator <- Maxent_Entity_Annotator(kind = var_entityType_string)
    
                                        # assemble the list of annotators into a processing
                                        # pipeline that will be used to configure the annotator
    var_pipeline_list <- list(obj_sentence_annotator, obj_word_annotator, obj_entity_annotator)

                                        # create the final model
    var_annotationModel_matrix <- annotate(var_textBlob_string, var_pipeline_list)
    
                                        # create an annotated doc.  This is a version of the document that is
                                        # represented as a structured hierarchy of sentences and words
    obj_annotatedText_document <- AnnotatedPlainTextDocument(var_textBlob_string, var_annotationModel_matrix)
    return(obj_annotatedText_document)
}


## showEnts <- function(annotatedDoc){
    
##     if (checkForEntities(obj_annotatedText_document, var_kind_character = var_entityType_string)) {

##                                         # get all entities of the type we are looking for
##         var_foundEntities_vector <- extractEntities(obj_annotatedText_document, var_kind_character = var_entityType_string)
        
##                                         # get vector of unique items
##         var_uniqueEntities_vector <- unique(var_foundEntities_vector)
        
##                                         # sort the entities vector
##         var_sortedEntities_vector <- sort(var_uniqueEntities_vector)
        
##         print("Sorted entity list:")
##         show_vector(var_sortedEntities_vector)
        
##         ## #################################
##         ## Code below is for cleaning list #
##         ## #################################
        
##                                         # review list of returned entities and create list
##                                         # of items entities that should be removed from the list
##         var_droplist_vector <- c("With", "Starbuck", "Watts", "Plato")
        
##                                         # now remove items from the droplist from the vector of
##                                         # extracted named entities
##         var_cleanedList_vector <- var_sortedEntities_vector[! var_sortedEntities_vector %in% var_droplist_vector ]
        
##         print("Filtered entity list:")
##         show_vector(var_cleanedList_vector)
        
##     } else {
##         print("No entities found")
##     }
## }
