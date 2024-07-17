//All the token clusters
var allClusters = []
var extractWordType = []

Shiny.addCustomMessageHandler('wordType', getWordType);
Shiny.addCustomMessageHandler('Handler', createClusters);
     
function getWordType(table) {
  for (var row in table.Word) {
    var wordType = {'word':table.Word[row], 'tag':table.EntityType[row]}
    extractWordType.push(wordType)
  }
  console.log(extractWordType)
}
 
function createClusters(mes) {
  allClusters = []
  var allTokens = []
  
  for (var featID in mes.feature) {
    var tokenData = {'word':mes.feature[featID], 'freq':mes.frequency[featID], 'group':mes.group[featID]}
    allTokens.push(tokenData)
  }
  console.log(allTokens)
  
  //Get all different groups
  var distinctGroups = [...new Set(mes.group)]

  //Filter allTokens by group
  for (var groupNum of distinctGroups) {
    try{
      var result = allTokens.filter(token => token.group == groupNum);
    }catch(err){
      console.log("Problem in the .filter")
    }
    
    allClusters.push(result)
  }
  
  //Clear older wordclouds
  //var wordcloudsDiv = document.getElementById('tagcloud_plot').innerHTML = "";
  /*var wordcloudsDiv = document.getElementById('tagcloud_plot')
  var allwordlcouds = document.querySelectorAll('.cluster_cloud')
  console.log(allwordlcouds.length)
  
  for (var div of allwordlcouds) {
    div.innerHTML = ""
  }
  
  while(wordcloudsDiv.firstChild) {
    wordcloudsDiv.removeChild(wordcloudsDiv.firstChild);
  }*/
}

console.log(allClusters)

//Table that relates tags to links and colors
var tagTypeColorLink = [
        {"tag":-1, "type": "PubChem Compound identifiers", "color":"#ffc000", "link":"https://pubchem.ncbi.nlm.nih.gov/compound/"}, //+id without 'CIDs'
        {"tag":-2, "type": "NCBI Taxonomy entries", "color": "#090088", "link": "https://www.ncbi.nlm.nih.gov/search/all/?term="}, //+word or taxonomy id (for tag>0)
        {"tag":-21, "type": "Gene Ontology biological process terms", "color": "#14b437", "link": "https://www.ebi.ac.uk/QuickGO/GTerm?id="}, //+id
        {"tag":-22, "type": "Gene Ontology cellular component terms", "color": "#79eb00", "link": "https://www.ebi.ac.uk/QuickGO/GTerm?id="}, //+id,
        {"tag":-23, "type": "Gene Ontology molecular function terms", "color": "#45980c", "link": "https://www.ebi.ac.uk/QuickGO/GTerm?id="}, //+id,
        {"tag":-25, "type": "BRENDA Tissue Ontology terms", "color":"#9620a4", "link":"http://www.ontobee.org/ontology/BTO?iri=http://purl.obolibrary.org/obo/"}, //+id, but use _ instead of :
        {"tag":-26, "type": "Disease Ontology terms", "color":"#e9705a", "link":"http://www.ontobee.org/ontology/DOID?iri=http://purl.obolibrary.org/obo/"}, //+id, but use _ instead of :
        {"tag":-27, "type": "Environment Ontology terms", "color":"#00b4b4", "link":"http://www.ontobee.org/ontology/ENVO?iri=http://purl.obolibrary.org/obo/"} //+id, but use _ instead of :
    ]
    
//LOCAL EXTRACT Words: Table that relates tags to colors and links
var extractOnlyTagColor = [
        {"tag":-1, "type": "PubChem Compound identifiers", "color":"#ffc000", "link":"https://pubchem.ncbi.nlm.nih.gov/#query="}, //+id without 'CIDs'
        {"tag":-2, "type": "NCBI Taxonomy entries", "color": "#090088", "link": "https://www.ncbi.nlm.nih.gov/search/all/?term="}, //+word or taxonomy id (for tag>0)
        {"tag":-21, "type": "Gene Ontology biological process terms", "color": "#14b437", "link": "https://www.ebi.ac.uk/QuickGO/search/"}, //+id
        {"tag":-22, "type": "Gene Ontology cellular component terms", "color": "#79eb00", "link": "https://www.ebi.ac.uk/QuickGO/search/"}, //+id,
        {"tag":-23, "type": "Gene Ontology molecular function terms", "color": "#45980c", "link": "https://www.ebi.ac.uk/QuickGO/search/"}, //+id,
        {"tag":-25, "type": "BRENDA Tissue Ontology terms", "color":"#9620a4", "link":"https://www.ontobee.org/search?ontology=&keywords="}, //+id, but use _ instead of :
        {"tag":-26, "type": "Disease Ontology terms", "color":"#e9705a", "link":"https://www.ontobee.org/search?ontology=&keywords="}, //+id, but use _ instead of :
        {"tag":-27, "type": "Environment Ontology terms", "color":"#00b4b4", "link":"https://www.ontobee.org/search?ontology=&keywords="} //+id, but use _ instead of :
    ]

//Create Checkboxes with entity entity_types
var entityDiv = document.getElementById('entityCheckboxes')

window.onload = function() {
  for (entityType of tagTypeColorLink) {
    var checkbox = document.createElement('input');
    checkbox.type = 'checkbox';
    checkbox.id = entityType["tag"];
    checkbox.addEventListener('change',selectEntity)
    checkbox.checked = 'checked'
    checkbox.className = 'entityColor'
    //checkbox.addEventListener("mouseover", func, false);
    //checkbox.addEventListener("mouseout", func1, false);
    //checkbox.name = 'interest';
    //checkbox.value = 'car';
 
    var label = document.createElement('label')
    label.htmlFor = entityType["tag"];
    label.style.color = entityType["color"];
    label.style.padding = "5px;"
    label.appendChild(document.createTextNode(entityType["type"]));
    //label.addEventListener("mouseover", func, false);
    //label.addEventListener("mouseout", func1, false);
 
    var br = document.createElement('br');
 
    //var container = document.getElementById('container');
    entityDiv.appendChild(checkbox);
    entityDiv.appendChild(label);
    entityDiv.appendChild(br);
  }
};

//Color only the entities selected by the user
const rgb2hex = (rgb) => `#${rgb.match(/^rgb\((\d+),\s*(\d+),\s*(\d+)\)$/).slice(1).map(n => parseInt(n, 10).toString(16).padStart(2, '0')).join('')}`

function selectEntity(){
  //console.log("selected")
  console.log(event.target.id)
  labels = document.getElementById('entityCheckboxes').getElementsByTagName('label');
  for( var i = 0; i < labels.length; i++ ) {
    if (labels[i].htmlFor == event.target.id) {
         var relevantLabel = labels[i];
    }
  }
  var labelColor = rgb2hex(relevantLabel.style.color);
  console.log(labelColor)
  var allCloudsDiv = document.getElementById('tagcloud_plot')
  var relevantCloudWords = allCloudsDiv.getElementsByClassName(labelColor);
  for (word of relevantCloudWords) {
    if (event.target.checked) {
      word.style.color = labelColor
    }else{
      word.style.color = '#8dabba'
    }
  }
}

function func() {  
  // not needed since item is already global, 
  // I am assuming this is here just because it's sample code?
  // var item = document.getElementById("button"); 
  //item.setAttribute("style", "background-color:blue;")
  //console.log(event.target.id)
  /*labels = document.getElementById('entityCheckboxes').getElementsByTagName('label');
  for( var i = 0; i < labels.length; i++ ) {
    if (labels[i].htmlFor == event.target.id) {
         var relevantLabel = labels[i];
    }
  }*/
  console.log(event.target.style.color)
  var labelColor = rgb2hex(event.target.style.color);
  console.log(labelColor)
  var allCloudsDiv = document.getElementById('tagcloud_plot')
  var relevantCloudWords = allCloudsDiv.getElementsByClassName(labelColor);
  for (word of relevantCloudWords) {
    word.style.backgroundColor = labelColor
    word.style.color = '#ffffff'
  }
}

function func1()
{  
  /*labels = document.getElementById('entityCheckboxes').getElementsByTagName('label');
  for( var i = 0; i < labels.length; i++ ) {
    if (labels[i].htmlFor == event.target.id) {
         var relevantLabel = labels[i];
    }
  }*/
  //console.log(event.target.style.color)
  var labelColor = rgb2hex(event.target.style.color);
  console.log(labelColor)
  var allCloudsDiv = document.getElementById('tagcloud_plot')
  
  var entityTypeBoxes = document.getElementsByClassName('entityColor')
  for( var i = 0; i < entityTypeBoxes.length; i++ ) {
    if (entityTypeBoxes[i].style.color == event.target.style.color) {
         var relevantEntity = entityTypeBoxes[i];
         console.log(relevantEntity)
    }
  }
  
  var relevantCloudWords = allCloudsDiv.getElementsByClassName(labelColor);
  for (word of relevantCloudWords) {
    word.style.backgroundColor = ''
    if (relevantEntity.checked) {
      word.style.color = labelColor
    }else{
      word.style.color = '#8dabba'
    }
  }
}


//Nice function to call
function buildWordcloud(db) {
    
  //Get font-normalizer factor
  var freqArray = db.map(function (el) { return el.freq; });
  
  var maxFreq = freqArray.reduce(function(a, b) {
    return Math.max(a, b);
  }, 0);
  
  /**var minFreq = freqArray.reduce(function(a, b) {
    return Math.min(a, b);
  }, 0);**/
  var minFreq = Math.min(...freqArray)
  
  var minFont = 15
  var maxFont = 70
  //console.log(minFreq)
  
  //var fontFactor = 50/maxFont
  var totalWords = 0

  var list = [];
  for (var i in db) {
    //list.push([db[i]["word"], (minFont + ((db[i]["freq"] - minFreq)*(maxFont-minFont)/(maxFreq - minFreq))]);
    let newFreq = minFont + ((db[i]["freq"] - minFreq)*(maxFont-minFont)/(maxFreq - minFreq))
    if (newFreq >= 15) {
      totalWords += 1
    }
    //console.log("New freq: "+newFreq)
    list.push([db[i]["word"], newFreq]);
  }

  //Get Identifiers for sample
  //sampleWithIDs = []

  var query = "http://tagger.jensenlab.org/GetEntities?document="
  for (var i = 0; i < db.length; i++) {
      let word = db[i]["word"]
      query += word.replace(/\s/g, '+')+"+" // Here I could add all the tokens and get results from extract
  }
  query += "&entity_types=-1+-2+-21+-22+-23+-25+-26+-27&auto_detect=1&format=xml" //auto_detect=1, returns more matches from extract but they are more abstract (beta-catenin => armadilo, which is the corresponding gene in drosophila)
  console.log(query)
  
    fetch(query)
    .then(response => response.text())
    .then(function (text) {

        var sampleWithIDs = []

        //Parse the xml
        var parser = new DOMParser();
        var xmlDoc = parser.parseFromString(text,"text/xml");
        var words = xmlDoc.getElementsByTagName("name")
        //var tags = xmlDoc.getElementsByTagName("type")
        //var IDs = xmlDoc.getElementsByTagName("identifier")
        //var items = xmlDoc.getElementsByTagName("item")
        var entities = xmlDoc.getElementsByTagName("entities")
        
        for (var j = 0; j < words.length; j++) {
            let element = {"word":words[j].childNodes[0].nodeValue, "tag":entities[j].childNodes[0].childNodes[0].childNodes[0].nodeValue, "ID":entities[j].getElementsByTagName("identifier")[0].innerHTML.replace('CIDs','').replace('BTO:','BTO_').replace('ENVO:','ENVO_').replace('DOID:','DOID_')}
            sampleWithIDs.push(element)
        }

        /*for (var j = 0; j < words.length; j++) {
            var element = {"word":words[j].childNodes[0].nodeValue, "tag":tags[j].childNodes[0].nodeValue, "ID":IDs[j].childNodes[0].nodeValue.replace('CIDs','').replace('BTO:','BTO_').replace('ENVO:','ENVO_').replace('DOID:','DOID_')}
            //sampleWithIDs.push(element)
        }*/
        console.log(sampleWithIDs)

        //Create a new div inside the main div, to put the wordcloud
        var allWordclouds = document.getElementById('tagcloud_plot')
        var clusterDiv = document.createElement('div');
        clusterDiv.className = 'cluster_cloud';
        clusterDiv.style = "width: 100%; height: 400px;"
    
        allWordclouds.appendChild(clusterDiv);

        //Generate the wordcloud
        //WordCloud.minFontSize = "20px"
        WordCloud(document.getElementById('tagcloud_plot').lastChild, { list: list, classes: "tag-cloud-item", gridSize: 10, rotateRatio:0, shrinkToFit:false} );

        const tagCanvas = document.getElementById('tagcloud_plot').lastChild; // select the div to draw in
        tagCanvas.addEventListener('wordcloudstop', function (e) {
            // loop over all added elements (by class name)
            tagCanvas.querySelectorAll('.tag-cloud-item').forEach(function (element) {

                //Get tag, ID of word
                var cloudWord = element.innerHTML;
                var wordInfo1 = sampleWithIDs.find(obj => {
                    return obj.word == cloudWord
                })
                
                try {
                    var wordTag = wordInfo1['tag']
                    var wordID = wordInfo1['ID']

                    //Get color, link for word
                    var wordColor = ""
                    var wordLink = ""
                    if (wordTag < 0) {
                        var wordInfo2 = tagTypeColorLink.find(obj => {
                            return obj.tag == wordTag
                        })
                        var wordColor = wordInfo2['color']
                        var wordLink = wordInfo2['link'] + wordID
                    }else{
                        var wordColor = "#807689"
                        var wordLink = "https://www.ncbi.nlm.nih.gov/search/all/?term=" + wordID
                        //console.log("Word: "+cloudWord+", tag:"+wordTag+", link: "+wordLink)
                    }
                    //Add a tailored link to every word
                    element.innerHTML = "<a href="+wordLink+" target='_blank' class="+wordColor+" rel='noopener noreferrer' style='color:"+wordColor+"'>"+cloudWord+"</a>";
                }catch(error) {
                    
                    
                    //The word is in local extract db but not in the online
                    if (extractWordType.length) {
                      console.log("Tricky word: "+cloudWord)
                      
                      //Get the words info(tag+name)
                      var extractWordInfo = extractWordType.find(obj => {
                        return obj.word == cloudWord
                      })
                      
                      var wordTag = extractWordInfo['tag']
  
                      //Get color, link for word
                      var wordColor = ""
                      var wordLink = ""
                      if (wordTag < 0) {
                          var wordInfo2 = extractOnlyTagColor.find(obj => {
                              return obj.tag == wordTag
                          })
                          var wordColor = wordInfo2['color']
                          var wordLink = wordInfo2['link'] + cloudWord
                      }else{
                          var wordColor = "#807689"
                          var wordLink = "https://www.ncbi.nlm.nih.gov/search/all/?term=" + cloudWord
                      }
                      
                      console.log(wordColor+wordLink)
                      
                      //Add a tailored link to every word
                      element.innerHTML = "<a href="+wordLink+" target='_blank' class="+wordColor+" rel='noopener noreferrer' style='color:"+wordColor+"'>"+cloudWord+"</a>";
                    
                  }else{
                  
                  //If the word isn't an Extract entity, it has this styling
                  element.innerHTML = "<span style='color:#8dabba'>"+cloudWord+"</span>";
                  }
              }
            });
        });

      //Remove the image added by shinny
      try{
        var image = allWordclouds.getElementsByTagName("IMG")[0];
        image.remove()
      }catch(err){
        console.log("Image already removed")
      }
    })

}