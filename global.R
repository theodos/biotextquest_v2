library(shinythemes)
library(vroom)
options(shiny.maxRequestSize = 700 * 1024^2)
library(excelR)
library(shinyWidgets)
#library(bslib)
library(shinyBS)
library(easyPubMed)
library(text2vec)
library(tokenizers)
library(MCL)
library(shinybusy)
library(ggfortify)
library(mongolite)
library(shinyjs)
library(textmineR)
library(SnowballC)
library(udpipe)
library(tidyr)
library(broom)
library(httr)
library(igraph)
library(dplyr)
library(stringr)
library(quanteda.textstats)
library(quanteda.textplots)
library(shinyjs)
#library(GScluster) 


# Default stopwords to be used. The user can also input his/her own.
stopwords = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")

# Prepossessing function for the words of a document used by text2vec package functions
prep_fun = tolower
# Tokenization function for the words of a document used by text2vec package functions
tok_fun = function(x) {
  quanteda::tokens(x, remove_punct = T, remove_symbols = F, remove_numbers = F, remove_url = T, remove_separators = F, split_hyphens = F)
  }
  

entity_choices_global = c("All" = "",
            "PubChem Compound identifiers" = "pubchem",
            "NCBI Taxonomy entries" = "ncbi",
            "Gene Ontology biological process terms" = "go_biological", 
            "Gene Ontology cellular component terms" = "go_cellular",
            "Gene Ontology molecular function terms" = "go_molecular",
            "BRENDA Tissue Ontology terms" = "brenda",
            "Disease Ontology terms" = "disease",
            "Environment Ontology terms" = "enviroment")


#   #function(x) {
#   #postag_lemma_tokenizer(x, udpipe_model=udpipe_load_model('/Users/theodos/Dropbox/TETRAKTYS_projects/Custom software/biotextquest/english-ewt-ud-2.5-191206.udpipe'),
#                                  tagger = "default",
#                                  tokenizer = "tokenizer", pos_keep = character(0),
#                                  pos_remove = character(0))
# }


themeelector <- function() {
  div(
    div(
      selectInput("shinytheme-selector", "Choose a theme",
                  c("default", shinythemes:::allThemes()),
                  selectize = FALSE
      )
    ),
    tags$script(
      "$('#shinytheme-selector')
        .on('change', function(el) {
        var allThemes = $(this).find('option').map(function() {
        if ($(this).val() === 'default')
        return 'bootstrap';
        else
        return $(this).val();
        });
        // Find the current theme
        var curTheme = el.target.value;
        if (curTheme === 'default') {
        curTheme = 'flatly';
        curThemePath = 'shinythemes/css/flatly.min.css';
        } else {
        curThemePath = 'shinythemes/css/' + curTheme + '.min.css';
        }
        // Find the <link> element with that has the bootstrap.css
        var $link = $('link').filter(function() {
        var theme = $(this).attr('href');
        theme = theme.replace(/^.*\\//, '').replace(/(\\.min)?\\.css$/, '');
        return $.inArray(theme, allThemes) !== -1;
        });
        // Set it to the correct path
        $link.attr('href', curThemePath);
        });"
    )
  )
}
