// The loading modal
var modal = document.getElementById("myModal");

// Get the button that opens the modal
//var btn = document.getElementById("myBtn");

// Get the <span> element that closes the modal
var span = document.getElementsByClassName("close")[0];

// When the user clicks on <span> (x), close the modal
//span.onclick = function() {
//  modal.style.display = "none";
//}

//Redraw wordclouds on resize
//window.onresize = changeWordcloudSize;

function changeWordcloudSize() {
  console.log("Resized")
  /*
  for (var cluster of allClusters) {buildWordcloud(cluster)}
  
  for (cloud of prevWordclouds) {
    cloud.remove();
  }*/
  
  //window.onload = redraw;

  /*var prevWordclouds = document.getElementsByClassName('cluster_cloud')
  console.log("previous: "+prevWordclouds.length)
  for (cloud of prevWordclouds) {
    cloud.remove();
  }*/
  
  //Shiny.setInputValue('resized', '100');
  
  /*var cloudSize = document.getElementById("totalWords").value
  var changedSize = cloudSize + 1
  Shiny.setInputValue('totalWords', '100');
  //Shiny.setInputValue('totalWords', cloudSize);
  console.log("OK")*/
  
  /*var initialClusters = document.getElementsByClassName('cluster_cloud')
  
  for (var cluster of allClusters) {buildWordcloud(cluster)}
  var allClouds = document.getElementsByClassName('cluster_cloud')
  console.log("allClouds= "+allClouds.length)
  console.log("initialClusters= "+initialClusters.length)
  
  var toRemove = allClouds - initialClusters*/
}

//Hide the Advanced div
/*var showButton = document.getElementById('toggle_advanced');
showButton.innerHTML = 'Show Advanced';
document.getElementById('advanced').hidden = true*/