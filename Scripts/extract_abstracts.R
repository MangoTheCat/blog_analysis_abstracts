extract_abstracts <- function(submission){
  # the original table has been read in as text but had two columns
  # first we need to find where the first column ends and the second starts
  # we do that using the "Submission Date"" element on the third row and find where the date starts
  column_split <- str_locate(submission[3], '[0-9]')[1, 'start']
  # now we can split the columns
  questions <- str_sub(submission, end=(column_split-1))
  answers <- str_sub(submission, start=column_split)
  # the abstract part is located between where the sentence begins with "Abstract" and
  # where it begins with "Suggest" (the next table element)
  begin <- str_which(submission, "Abstract")
  end <- str_which(submission, "Suggest")
  # The "Suggest" element was optional so could be missing - don't you just love messy data
  if(length(end)==0){
    end <- str_which(submission, "Target")
  }
  abstract <- answers[begin:(end-1)] %>% 
    # some cleaning of the text
    str_to_lower() %>% 
    str_trim()
  
  # removing  empty lines, don't know how to do that in the pipe  
  abstract <- abstract[abstract!=""]
  # finally paste everything together into one string
  final_abstract <- paste(abstract, collapse=" ")
  
  return(data.frame(Title=extract_title(submission, column_split), 
                    Abstract=final_abstract, stringsAsFactors = FALSE))
}

extract_title <- function(submission, column_split){
  # similarly as the above function
  # find the element that contains the Title
  begin <- str_which(submission, "Title")
  end <- str_which(submission, "Category")
  if (length(end) == 0){
    # fail safe if there is no Category element
    end <- str_which(submission, "Abstract") 
  }
  # if multiple elements containt Category/Abstract, pick the first one
  if (length(end) > 1){
    end <- end[1]
  }
  # split the questions and answers (the column split comes from the above function)
  answers <- str_sub(submission, start=column_split)
  # some text wrangling to put the title in a uniform format
  title <- answers[begin:(end-1)] %>% 
    str_c(collapse=" ") %>% 
    str_replace("Title of Your Session:", "") %>% 
    str_trim() %>% 
    str_to_lower()
  
  title <- remove_punctuation(title)  
  
  return(title)
}

remove_punctuation <- function(text){
  # remove the following characters from the given text
  punct <- '[]\\?!\"\'#$%&(){}+*/:;,._`|~\\[<=>@\\^-]'
  return(str_replace_all(text, punct, ""))
}