function required_field(id) {
  if(document.getElementById(id).value === "") {
    document.getElementById(id).style.backgroundColor = "#ed9194";
  } else {
    document.getElementById(id).style.backgroundColor = "white";
  }
}

function get_parameters(id, target) {
  table = document.getElementById(id);
  rows = table.rows.length;
  cols = table.rows[0].cells.length;
  
  content = "";
  for (let i = 1; i <= cols; i++) {
    elem = document.getElementById(id + "_" + i);
    if(elem.value === undefined) { // no textinput
      content = content + elem.innerHTML;
    } else {
      content = content + elem.value;
    }
    if(i != cols) {
      content = content + ";=;";
    }
  }
  content = content + ";+;";
  
  for (let i = 1; i < rows; i++) {
    for(let j = 1; j <= cols; j++) {
      elem = document.getElementById(id + "_" + i + "_" + j);
      if(elem.value === undefined) { // no textinput
        content = content + elem.innerHTML;
      } else {
        content = content + elem.value;
      }
      if(j != cols) {
        content = content + ";=;";
      }
    }
    if(i != rows-1) {
      content = content + ";+;";
    }
  }
  
  Shiny.setInputValue(target, content);
  
  //document.getElementById(target).value = content;
}

Shiny.addCustomMessageHandler('get_parameter_input', function(message) {
  get_parameters(message[0], message[1]);
});

Shiny.addCustomMessageHandler('set_active_tab', function(message) {
	
	if(message[0] != -1) {
		setTimeout(function(){
			$('.treeview > a').eq(message[0]).click();
		}, 200);
	}
	
	setTimeout(function(){
        $('.treeview-menu > li > a').eq(message[1]).click();
    }, 800);
});

function set_active_tab(main, sub) {
	if(main != -1) {
		setTimeout(function(){
			$('.treeview > a').eq(main).click();
		}, 200);
		
		setTimeout(function(){
        $('.treeview-menu > li > a').eq(sub).click();
		}, 800);
	} else {
		
		setTimeout(function(){
        $('.treeview-menu > li > a').eq(sub).click();
		}, 200);
	}
	
	
}

function expand(e) {
	e.classList.toggle("active_tab");
    var content = e.nextElementSibling;
    if (content.style.display === "block") {
      content.style.display = "none";
    } else {
      content.style.display = "block";
    }
  }