<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Multiagent NetLogo Models</title>
<link rel="stylesheet" type="text/css" href="style.css"/>
</head>
<body>
<center>
	<h1>Multiagent NetLogo Models</h1>
</center>

<div id="grid">
<#list models.models as m>
<div class="model">
  <div class="thumbHolder">
    <a href="${m.modelName}/index.html"><img src="${m.modelName}/thumb.png"/></a>
  </div>
  <h4><a href="${m.modelName}/index.html">${m.title}</a></h4>
  <h5>${m.credits} </h5>
</div>
</#list>
</div>

</body>
</html>