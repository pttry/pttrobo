function getVerticalLayout(el, legend_fontsize, height = false, keys, showfinal = false) {
  if(height == false) {height = el.layout.height};
	let elcontainer = height;
	let elslider = 0;
	if ('rangeslider' in el.layout.xaxis) {
	  if(el.layout.xaxis.rangeslider.visible == true) {
	    elslider = $(el).find('g.infolayer > g.rangeslider-container')[0].getBBox().height
	  }
	  }
	let elxticks = $(el).find('g.xaxislayer-above')[0].getBBox().height;
	let margin_top = ($(el).find('g.g-gtitle')[0].getBBox().height)+el.layout.title.font.size;
	let elcaption = $(el).find('g.annotation')[0].getBBox().height;
	let ellegend = $(el).find('g.legend')[0].getBBox().height;
	let margin_bottom = ellegend + elcaption + elxticks;
	let margin_bottom_disp = elslider * 1//elslider > 0 ? margin_bottom * 0.1 : 0;
	let elplot = $(el).find('.cartesianlayer > .xy > .gridlayer > .x')[0].getBBox().height;
	let images_sizey = elcontainer / 20 / elplot;
	let elimages = $(el).find('g.layer-above > g.imagelayer')[0].getBBox().height;
	let legend_y = -((margin_bottom - elcaption + (elslider+margin_bottom_disp)) / elplot);
	if (legend_y < -3) {
	  Plotly.relayout(el, {'xaxis.rangeslider.visible': false})
	  elslider = 0;
	  elxticks = $(el).find('g.xaxislayer-above')[0].getBBox().height;
  	ellegend = $(el).find('g.legend')[0].getBBox().height;
	  margin_bottom = ellegend + elcaption + elxticks;
	  margin_bottom_disp = 0//elslider > 0 ? margin_bottom * 0.1 : 0;
	  elplot = $(el).find('.cartesianlayer > .xy > .gridlayer > .x')[0].getBBox().height;
	  images_sizey = elcontainer / 20 / elplot;
	  elimages = $(el).find('g.layer-above > g.imagelayer')[0].getBBox().height;
	  legend_y = -((margin_bottom - elcaption + (elslider+margin_bottom_disp)) / elplot);
	}
	let annotations_y = -((margin_bottom + (elslider+margin_bottom_disp) - legend_fontsize * 0.3) / elplot);
	let images_y = -((margin_bottom + (elslider+margin_bottom_disp) - legend_fontsize * 0.3 - (elimages / 10)) / elplot);
	let legend_font_size = (ellegend > (elplot / 2)) ? legend_fontsize - 2 : legend_fontsize;
	if(showfinal == true) {console.log('legend ht: ' + ellegend +
	' slider ht: ' + elslider +
	' plot area ht: ' + elplot +
	' bottom margin: ' + margin_bottom +
	' container ht: ' + elcontainer,
	' legend position: ' + legend_y, 	
	' caption position: ' + annotations_y, 
	)}
	let thearray = {
		'legend.y': legend_y,
		'legend.yanchor': "bottom",
		'images[0].y': images_y,
		'images[0].sizey': images_sizey,
		'margin.t': margin_top,
		'margin.b': margin_bottom,
		'annotations[0].y': annotations_y,
		'annotations[1].y': annotations_y,
		'annotations[2].y': annotations_y,
		'legend.font.size': legend_font_size
	};
//	const inclusivePick = (obj, keys) => Object.fromEntries(
//  keys.map(key => [key, obj[key]]));
  
  let rearray = keys.reduce(function (obj2, key) {
  if (key in thearray) // line can be removed to make it inclusive
    obj2[key] = thearray[key];
    return obj2;
    
  }, {});


//  let rearray = inclusivePick(thearray, keys)
	return rearray;

}

function setVerticalLayout(eventdata, gd, legend_fontsize) {
	if ('width' in eventdata) {
	  if ('rangeslider' in gd.layout.xaxis) {
	  if (gd.layout.xaxis.rangeslider.visible == false) {
	    gd.layout.xaxis.rangeslider.visible = true;
	    Plotly.react(gd,gd.data, gd.layout)
	    //Plotly.relayout(gd, {'xaxis.rangeslider.visible': true})
	  }}
		Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['legend.font.size','images[0].sizey']));
		Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['margin.t','margin.b','legend.y','images[0].y']));
		Plotly.relayout(gd, getVerticalLayout(gd, legend_fontsize, false, keys = ['margin.t', 'margin.b','images[0].sizey']));
		Plotly.relayout(gd, 
		  getVerticalLayout(gd, legend_fontsize, false, keys = ['images[0].y', 'annotations[0].y', 'annotations[1].y', 'annotations[2].y','legend.y', 'legend.yanchor']));
//		console.log("FINAL CHECK")
//		getVerticalLayout(gd, legend_fontsize, false, [""], showfinal = true);
	}
}

function yrangeRelayout(eventdata, gd, timerId) {
	if (Object.prototype.toString.call(eventdata['xaxis.range']) === '[object Array]') {
		//console.log(\"rangeslider event!!\");
		var xRange = gd.layout.xaxis.range;
		var yRange = gd.layout.yaxis.range;
		var yInside = [];
		var xInside = [];
		console.log('yrange min: ' + Math.min(...yRange))
		var visdata = gd.data.filter(trace => trace.visible === true || !(trace.hasOwnProperty('visible')));
		visdata.forEach(trace => {
			var len = Math.min(trace.x.length, trace.y.length);

			for (var i = 0; i < len; i++) {
				var x = trace.x[i];
				var y = trace.y[i];

				if (x >= xRange[0] && x <= xRange[1]) {
					xInside.push(x);
					yInside.push(y);
				}
			}
		});
		console.log('yinside min: ' + Math.min(...yInside))
		let yMax = Math.max(...yInside);
		yMax = yMax < 0 ? yMax * 0.95 : yMax * 1.05;
		let yMin = Math.min(...yInside);
		yMin = yMin < 0 ? yMin * 1.05 : yMin * 0.95;
		var update = {
			'yaxis.range': [yMin,yMax], 'shapes[0].x0': xRange[0]  // updates the end of the yaxis range
		};
		Plotly.relayout(gd, update);
		if (timerId >= 0) {
			//timer is running: stop it
			window.clearTimeout(timerId);
		}
		timerId = window.setTimeout(function() {
			//fire end event
			//console.log('rangeslider event ENDS');
			//reset timer to undefined
			timerId = -1;
		}, 800);
	}
}

function plotlyRelayoutEventFunction(eventdata, gd, legend_fontsize) {
	timerId = 0;
	setVerticalLayout(eventdata, gd, legend_fontsize);
	yrangeRelayout(eventdata, gd, timerId);
};