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

# Default stopwords to be used. The user can also input his/her own.
stopwords = c("i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours")

# Prepossessing function for the words of a document used by text2vec package functions
prep_fun = tolower
# Tokenization function for the words of a document used by text2vec package functions
tok_fun = word_tokenizer


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
