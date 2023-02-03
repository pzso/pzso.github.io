/***********************************************
* JavaScriptKit.com Multiple Choice Quiz Script (http://www.javascriptkit.com)
* Copyright 2003 JavaScript Kit- http://www.javascriptkit.com
* This notice and footnote must stay intact for use
* Visit JavaScript Kit (http://www.javascriptkit.com/) for full source code
***********************************************/

//correctchoices

var totalquestions=10

var correctchoices=new Array()
correctchoices[1]='a' //question1
correctchoices[2]='c' //question2
correctchoices[3]='a' //question3
correctchoices[4]='b' //question4
correctchoices[5]='b' //question5
correctchoices[6]='c' //question6
correctchoices[7]='d' //question7
correctchoices[8]='c' //question8
correctchoices[9]='c' //question9
correctchoices[10]='b' //question10

function gradeit(){
var incorrect=null
for (q=1;q<=totalquestions;q++){
 var thequestion=eval("document.myquiz.question"+q)
 for (c=0;c<thequestion.length;c++){
 if (thequestion[c].checked==true)
 actualchoices[q]=thequestion[c].value
 }
 
 if (actualchoices[q]!=correctchoices[q]){ //process an incorrect choice
 if (incorrect==null)
 incorrect=q
 else
 incorrect+="/"+q
 }
 }

if (incorrect==null)
incorrect="a/b"
document.cookie='q='+incorrect
if (document.cookie=='')
alert("A b&ouml;ng&eacute;sz&#337;ben enged&eacute;lyezni kell a s&uuml;tik (cookie) fogad&aacute;s&aacute;t.")
else
window.location="results.htm"
}


function showsolution(){
var win2=window.open("","win2","width=200,height=350, scrollbars")
win2.focus()
win2.document.open()
win2.document.write('<title>Megold&aacute;s</title>')
win2.document.write('<body bgcolor="#FFFFFF">')
win2.document.write('<center><h3>A teszt megold&aacute;sai</h3></center>')
win2.document.write('<center><font face="Arial">')
for (i=1;i<=totalquestions;i++){
for (temp=0;temp<incorrect.length;temp++){
if (i==incorrect[temp])
wrong=1
}
if (wrong==1){
win2.document.write(i+". k&eacute;rd&eacute;s = "+correctchoices[i].fontcolor("red")+"<br>")
wrong=0
}
else
win2.document.write(i+". k&eacute;rd&eacute;s = "+correctchoices[i]+"<br>")
}
win2.document.write('</center></font>')
win2.document.write("<h5>Megjegyz&eacute;s: piros sz&iacute;nnel jel&ouml;lve a helytelen&uuml;l megv&aacute;laszolt.</h5><p align='center'><small><a href='http://www.javascriptkit.com' target='_new'>JavaScript Kit quiz script</a></small>")
win2.document.close()
}