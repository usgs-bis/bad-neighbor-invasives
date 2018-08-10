var list = "<i>"
var t1 ="";
var t2 ="";

function jsonp(url) 
{
var head = document.head;
var script = document.createElement("script");
script.setAttribute("src", url);
head.appendChild(script);
head.removeChild(script);
}

function badN()
{
list = "<i>"
t1 ="";
t2 ="";
bufferSpp =""
var t = (document.forms.getState.state.value);
var g = (document.forms.getState.group.value);
t1 = (t.slice(2))
var x = ("https://data.usgs.gov/solr/occurrences/select/?q=establishmentMeans:L48 AND computedStateFips:")
var y = ("&facet=true&facet.field=ITISscientificName&rows=0&facet.limit=-1&facet.mincount=1&wt=json&json.nl=arrarr&json.wrf=");
var z = "buffer"
jsonp(x+t1+g+y+z);
}

function buffer(x)
{
var e = x.facet_counts.facet_fields.ITISscientificName.length;
var e2 = (x.facet_counts.facet_fields.ITISscientificName);
var e3 = e2.length;
var e4 = JSON.stringify(e2);
bufferSpp = JSON.parse(e4);
state();
}

function state()
{
var t = (document.forms.getState.state.value);
var g = (document.forms.getState.group.value);
t2 = (t.substr(0,2))
var x = ("https://data.usgs.gov/solr/occurrences/select/?q=establishmentMeans:L48 AND computedStateFips:")
var y = ("&facet=true&facet.field=ITISscientificName&rows=0&facet.limit=-1&facet.mincount=1&wt=json&json.nl=arrarr&json.wrf=");
var z1 = "BNList"
jsonp(x+t2+g+y+z1);
}

function BNList(y)
{
var count = 0;
var e = y.facet_counts.facet_fields.ITISscientificName.length;
var e2 = (y.facet_counts.facet_fields.ITISscientificName);
var e3 = e2.length;
var e4 = JSON.stringify(e2);
stateSpp = JSON.parse(e4);
for (var t=0;t<stateSpp.length;t++)
{
for (var t1=0;t1<bufferSpp.length;t1++)
{
if (stateSpp[t][0] == bufferSpp[t1][0])
{
bufferSpp[t1][0] = "delete";
}
}
}
for (var t2=0;t2<bufferSpp.length;t2++)
{
if (bufferSpp[t2][0] != "delete")
{
count = count+1;
var spp = bufferSpp[t2][0]

var spc = species[spp]

list = (list + "<br><a href ='https://bison.usgs.gov/index.jsp?scientificName=" + spp + "&ITIS=itis' target='_blank'>" + spp +"</a> " + spc);
}
}
document.getElementById("listHTML3").innerHTML = ("There are "+ count + " species of non-natives documented in states bordering this state that are not documented in the state itself in <a href='https://bison.usgs.gov' target='_blank'>BISON</a>.<br>Click on a name to open a tab with the species full distribution in <a href='https://bison.usgs.gov' target='_blank'>BISON</a>.<br>" + list + "</i>");
}