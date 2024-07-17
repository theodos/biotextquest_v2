//All the token clusters
var allClusters = []

Shiny.addCustomMessageHandler('wordType', createWordType);
Shiny.addCustomMessageHandler('Handler', createClusters);

//Download wordclouds
$("#btnConvert").on('click', function () {
  
  var allWordclouds = document.getElementsByClassName("cluster_cloud")
  var clusterCount = 1
  
  var HTML_Width = allWordclouds[0].offsetWidth;
  var HTML_Height = allWordclouds[0].offsetHeight;
  var PDF_Width = HTML_Width + (30 * 2);
  var PDF_Height = HTML_Height + 60
  //const pdf = new jsPDF('p', 'pt', [800+PDF_Width, PDF_Height])
  /*const pdf = new jsPDF()
    
 for (var wordCloud of allWordclouds){
		/*html2canvas(wordCloud,		{
			allowTaint: true,
			useCORS: true
		}).then(function (canvas) {
			var anchorTag = document.createElement("a");
			document.body.appendChild(anchorTag);
			//document.getElementById("previewImg").appendChild(canvas);
			let filename = "wordCloud"+clusterCount.toString()+".jpg"
			
			//new
			var doc = new jsPDF();

      doc.text(10, 10, 'Hello world!');
      
      doc.addImage(filename, 'JPEG', 10, 30, 150, 76);
      
      doc.save('a4.pdf');
      			
			anchorTag.download = filename;
			anchorTag.href = canvas.toDataURL();
			anchorTag.target = '_blank';
			anchorTag.click();*/
	    /*const printArea = wordCloud
      html2canvas(printArea).then(canvas => {
      const imgData = canvas.toDataURL('image/png');
      //pdf.addPage(400, 300);
      //pdf.addPage(PDF_Width, PDF_Height);
      //pdf.addImage(imgData, 'JPG', 30, -(PDF_Height*clusterCount)+(30*4),HTML_Width,HTML_Height);
      //pdf.addImage(imgData, 'JPG', wordCloud.offsetHeight, wordCloud.offsetWidth)
      pdf.addImage(imgData, 'JPG', 10, 30, wordCloud.offsetWidth/4, wordCloud.offsetHeight/4)
      pdf.save("wordCloud"+clusterCount.toString());
			clusterCount ++
		});
		
  }*/
  const pdf = new jsPDF()

  for (var wordCloud of allWordclouds){
    const printArea = wordCloud
    html2canvas(printArea).then(canvas => {
      const imgData = canvas.toDataURL('image/png');
  
      // Add a new page to the PDF
      pdf.addPage()
      // Set the current page number
      pdf.setPage(clusterCount)
      // Add the image to the PDF
      pdf.addImage(imgData, 'JPG', 10, 30, wordCloud.offsetWidth/5, wordCloud.offsetHeight/5)
      if (clusterCount == allWordclouds.length){
        var pageCount = pdf.internal.getNumberOfPages();
        pdf.deletePage(pageCount)
        pdf.save("WordClouds");
      }
      clusterCount ++
    });
  }


  
});
var wordTagID = [];
function createWordType(mes){
  console.log(mes)
  wordTagID = []

  // Create a new empty list to store the converted data.
  //const convertedList = [];

  // Iterate over the Word and EntityType arrays in the original object and add each pair of elements to the   new list as a two-property object.
  for (let i = 0; i < mes.Word.length; i++) {
    wordTagID.push({
      'word': mes.Word[i],
      'tag': mes.EntityType[i],
      'id': mes.Ext_db_id[i]
    });
  }
  
  // Return the new list.
  //wordTag = convertedList;
  console.log(wordTagID)
  //return wordTag
  }
      
function createClusters(mes, db="Pubmed", additionalTable="None") {
  allClusters = []
  var allTokens = []
  
  //console.log(wordTag)
  console.log(mes.feature)
  
  for (var featID in mes.feature) {
    
    //Get the tag from wordTagID list
    var wordInfo = wordTagID.find(obj => {return obj.word.toLowerCase() == mes.feature[featID].toLowerCase()})
    //console.log(wordInfo)
    //if (!wordInfo){
      //var wordInfo = {'tag':null}
    //}
    //var wordInfo = wordTag.find(obj => {return obj.word.toLowerCase() == mes.feature[featID].toLowerCase()})
    //console.log(wordInfo)
    try{
      tokenData = {'word':mes.feature[featID], 'freq':mes.frequency[featID], 'group':mes.group[featID], 'tag':wordInfo['tag'], 'id':wordInfo['id']}
    }catch{
       tokenData = {'word':mes.feature[featID], 'freq':mes.frequency[featID], 'group':mes.group[featID]}
    }
    allTokens.push(tokenData)
  }
  //console.log(allTokens)
  
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
  var wordcloudsDiv = document.getElementById('tagcloud_plot')
  var allwordlcouds = document.querySelectorAll('.cluster_cloud')
  console.log(allwordlcouds.length)
  
  for (var div of allwordlcouds) {
    div.innerHTML = ""
  }
  
  while(wordcloudsDiv.firstChild) {
    wordcloudsDiv.removeChild(wordcloudsDiv.firstChild);
  }
}

console.log(allClusters)
console.log("Inner2")

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
    
//Table that relates tags to colors to links WITHOUT using ID (when extract API does not work)
var tagTypeColorLink_noID = [
        {"tag":-1, "type": "PubChem Compound identifiers", "color":"#ffc000", "link":"https://pubchem.ncbi.nlm.nih.gov/#query="}, //+word
        {"tag":-2, "type": "NCBI Taxonomy entries", "color": "#090088", "link": "https://www.ncbi.nlm.nih.gov/search/all/?term="}, //+word
        {"tag":-21, "type": "Gene Ontology biological process terms", "color": "#14b437", "link": "https://www.ebi.ac.uk/QuickGO/search/"}, //+word
        {"tag":-22, "type": "Gene Ontology cellular component terms", "color": "#79eb00", "link": "https://www.ebi.ac.uk/QuickGO/search/"}, //+word
        {"tag":-23, "type": "Gene Ontology molecular function terms", "color": "#45980c", "link": "https://www.ebi.ac.uk/QuickGO/search/"}, //+word
        {"tag":-25, "type": "BRENDA Tissue Ontology terms", "color":"#9620a4", "link":"https://ontobee.org/search?ontology=BTO&keywords="}, //+word
        {"tag":-26, "type": "Disease Ontology terms", "color":"#e9705a", "link":"https://ontobee.org/search?ontology=DOID&keywords="}, //+word
        {"tag":-27, "type": "Environment Ontology terms", "color":"#00b4b4", "link":"https://ontobee.org/search?ontology=ENVO&keywords="} //+word
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
    label.addEventListener("mouseover", func, false);
    label.addEventListener("mouseout", func1, false);
 
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
  labels = document.getElementById('entityCheckboxes').getElementsByTagName('label');
  for( var i = 0; i < labels.length; i++ ) {
    if (labels[i].htmlFor == event.target.id) {
         var relevantLabel = labels[i];
    }
  }
  var labelColor = rgb2hex(relevantLabel.style.color);
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
  var labelColor = rgb2hex(event.target.style.color);
  //console.log(labelColor)
  var allCloudsDiv = document.getElementById('tagcloud_plot')
  var relevantCloudWords = allCloudsDiv.getElementsByClassName(labelColor);
  for (word of relevantCloudWords) {
    word.style.backgroundColor = labelColor
    word.style.color = '#ffffff'
  }
}

function func1()
{  
  //Find relevant Checkbox
  checkboxes = document.getElementsByClassName('entityColor');
  for( var i = 0; i < checkboxes.length; i++ ) {
    if (checkboxes[i].id == event.target.htmlFor) {
         relevantCheckbox = checkboxes[i];
    }
  }
  //var labelColor = rgb2hex(event.target.style.color);
  var labelColor = event.target.style.color;
  var allCloudsDiv = document.getElementById('tagcloud_plot')

  var allCloudWords = allCloudsDiv.getElementsByTagName('a')
  for (let word of allCloudWords) {
    
    if (word.style.backgroundColor == labelColor){
      if (relevantCheckbox.checked){
        word.style.color = labelColor
        word.style.backgroundColor = ''
      }else{
        word.style.color = '#8dabba'
        word.style.backgroundColor = ''
        
      }
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
      
      //Check for NaN (TODO: check why NaNs are created, maybe 1-1 =0 and div by 0)
    }else if (isNaN(newFreq)) {
      newFreq = 20
      totalWords +=1 
    }
    
    var term = db[i]["word"]+"("+db[i]["freq"]+")"
    //console.log(db[i]["word"]+"("+db[i]["freq"]+")")
    //list.push([db[i]["word"], newFreq]);
    list.push([term, newFreq]);
  }

  console.log(list)
  console.log(db)
  //Get Identifiers for sample
  //sampleWithIDs = []

  var query = "https://tagger.jensenlab.org/GetEntities?document="
  for (var i = 0; i < db.length; i++) {
      let word = db[i]["word"]
      query += word.replace(/\s/g, '+')+"+" // Here I could add all the tokens and get results from extract
  }
  //query += "&auto_detect=1&format=xml"
  query += "&entity_types=-1+-2+-21+-22+-23+-25+-26+-27+9606+10090+7227&auto_detect=1&format=xml" //auto_detect=1, returns more matches from extract but they are more abstract (beta-catenin => armadilo, which is the corresponding gene in drosophila)
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
        //clusterDiv.style = "width: 100%; height: 400px;"
    
        allWordclouds.appendChild(clusterDiv);
        
        //Add on-hover tag that shows the number of the cluster
        const allClusterDivs = document.querySelectorAll(".cluster_cloud");
        
        clusterDiv.addEventListener("mouseover", () => {
          document.getElementById("focusCloud").innerHTML="Wordcloud in focus: Cluster "+allClusterDivs.length
        
          document.getElementById("focusCloud").classList.add("flash");

          // Remove the flash class after a short timeout (adjust as needed)
          setTimeout(() => {
            document.getElementById("focusCloud").classList.remove("flash");
          }, 500);
        });
        
        
        /*//Create a new div inside the main div, to put the wordcloud
        var allWordclouds = document.getElementById('tagcloud_plot')
        var separateCloud = document.createElement('div');
        separateCloud.className = 'cloud_subdiv';
        
        allWordclouds.appendChild(separateCloud);
        
        var clusterDiv = document.createElement('div');
        clusterDiv.className = 'cluster_cloud';
        //clusterDiv.style = "width: 100%; height: 400px;"
        
        // Create the h2 element for the wordcloud
        var heading = document.createElement('h2');
        heading.textContent = 'Wordcloud Heading'; // Replace this with your desired heading text
        
        var subdiv = document.getElementById('tagcloud_plot').lastChild
        console.log(subdiv)
        subdiv.appendChild(heading);
        subdiv.appendChild(clusterDiv);*/
        
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
                    return obj.word.toLowerCase() == cloudWord.toLowerCase()
                })

                try {
                    var wordTag = wordInfo1['tag']
                    var wordID = wordInfo1['ID']
                    //console.log("ID: "+wordID)
                    //console.log("Tag: "+wordTag)

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
                    // Parse the font size as a number (might be decimal)
                    //let fontSizeNumber = parseFloat(element.style.fontSize);
                  
                    // Round the number using Math.round()
                    //let roundedFontSize = Math.round(fontSizeNumber);
                    //element.innerHTML = "<a href="+wordLink+" target='_blank' class="+wordColor+" rel='noopener noreferrer' style='color:"+wordColor+"'>"+cloudWord+"("+roundedFontSize+")"+"</a>";
                    element.innerHTML = "<a href="+wordLink+" target='_blank' class="+wordColor+" rel='noopener noreferrer' style='color:"+wordColor+"'>"+cloudWord+"</a>";
                    //Do not display single letter words
                    if (cloudWord.length < 2){
                      element.style.display="none"
                    }
                }catch(error) {
                  
                    //The word is in local extract db but not in the online
                    if (db == "Extract") {
                      console.log("Tricky word"+cloudWord)
                    }
                    //If extract does not work:
                    //console.log(db)
                    var wordInfo1 = db.find(obj => {
                      return obj.word.toLowerCase() == cloudWord.toLowerCase()
                    })
                    
                    try {
                      var wordTag = wordInfo1['tag']
                      var wordID = wordInfo1['id'].replace('CIDs','').replace('BTO:','BTO_').replace('ENVO:','ENVO_').replace('DOID:','DOID_')
                      //console.log("ID: "+wordID)
                      //console.log("Tag: "+wordTag)
  
                      //Get color, link for word
                      var wordColor = ""
                      var wordLink = ""
                      if (wordTag < 0) {
                          var wordInfo2 = tagTypeColorLink.find(obj => {
                              return obj.tag == wordTag
                          })
                          var wordColor = wordInfo2['color']
                          var wordLink = wordInfo2['link'] + wordID//cloudWord.replace(/\s+/g, '%20')
                      }else{
                          var wordColor = "#807689"
                          var wordLink = "https://www.ncbi.nlm.nih.gov/search/all/?term=" + cloudWord.replace(/\s+/g, '%20')
                          //console.log("Word: "+cloudWord+", tag:"+wordTag+", link: "+wordLink)
                      }
                      //Add a tailored link to every word
                      element.innerHTML = "<a href="+wordLink+" target='_blank' class="+wordColor+" rel='noopener noreferrer' style='color:"+wordColor+"'>"+cloudWord+"</a>";
                      //Do not display single letter words
                      if (cloudWord.length < 2){
                        element.style.display="none"
                      }
                  }catch(error) {
                    //If the word isn't an Extract entity, it has this styling
                      element.innerHTML = "<span style='color:#8dabba'>"+cloudWord+"</span>";
                      //Do not display single letter words
                      if (cloudWord.length < 2){
                        element.style.display="none"
                      }
                  }
                    
                }//catch
            });
        });

      //Resize the div to fit the words
      //Width
      var clusterWidth = clusterDiv.offsetWidth
      clusterWidth += 20;
      clusterDiv.style.width = clusterWidth+"px"
      
      //Height
      var clusterHeight = clusterDiv.offsetHeight
      clusterHeight += 10;
      clusterDiv.style.height = clusterHeight+"px"

      //Remove the image added by shinny
      try{
        var image = allWordclouds.getElementsByTagName("IMG")[0];
        image.remove()
      }catch(err){
        console.log("Image already removed")
      }
    })

}