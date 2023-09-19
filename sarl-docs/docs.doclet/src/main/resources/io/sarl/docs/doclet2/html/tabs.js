function openTabElement(button,tabName,tabButtonGroupName,tabContentGroupName) {
  var i;
  var x = document.getElementsByClassName(tabContentGroupName);
  for (i = 0; i < x.length; i++) {
    x[i].style.display = "none";
  }
  var y = document.getElementsByClassName(tabButtonGroupName);
  for (i = 0; i < y.length; i++) {
    y[i].className = y[i].className.replace(" active", "");
  }
  document.getElementById(tabName).style.display = "block";
  button.className += " active"
}
