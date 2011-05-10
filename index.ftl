<!DOCTYPE html>
<html>
<head>
<meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
<title>Multiagent NetLogo Models</title>
<link rel="stylesheet" type="text/css" href="style.css"/>
</head>
<body>
<div id="page">
  <div id="header">
    <h4 id="title">Multiagent Netlogo Models</h4>
    <ul id="nav">
      <li><a id="aboutLink" href="#">About</a></li>
      <li><a href="http://jmvidal.cse.sc.edu">Author</a></li>
      <li><a
      href="http://ccl.northwestern.edu/netlogo/">NetLogo</a></li>
      <li><a href="http://www.multiagent.com">MultiAgent Systems</a></li>
    </ul>
  </div>
  <div id="featuredSection">
    <div id="featuredModel">
    <a href="${models.models?first.modelName}/index.html"><img class="header" src="${models.models?first.modelName}/medium.png"/></a>
    <dl id="featuredVideo">
      <dt><a href="${models.models?first.modelName}/index.html">${models.models?first.title}</a></dt>
      <dd>${models.models?first.indexdescription}</dd>
    </dl>
    </div>
    <div id="aboutText">
      <p>These are NetLogo models that demonstrate various well known
      MultiAgent algorithms and other related techniques. They have
      been developed by <a
      href="http://jmvidal.cse.sc.edu">myself</a>, my students, and
      others over the years.</p>
      <p>Many of these models implement algorithms described in detail
      in my free textbook <a
      href="http://www.multiagent.com">Fundamentals of Multiagent
      Systems</a>.</p>
      <p>If you need a consultant or just some help in building a
      NetLogo model for your research, let me know. Some of these
      models, and many others not shown here, are the results of
      collaborations with other researchers in various science and
      engineering disciplines.</p>
   </div>
  </div>
  
<div id="grid">
<#list models.models as m>
  <#if m_index &gt; 0 >
<div class="model">
  <div class="thumbHolder">
    <a onclick="popup('${m.modelName}',${m.width},${m.height});return false;" href="#"><img src="${m.modelName}/thumb.png"/></a>
  </div>
  <h4><a class="modelLink" id="${m.modelName}"
  href="${m.modelName}/index.html" onclick="popup('${m.modelName}',${m.width},${m.height});return false;">${m.title}</a></h4>
  <div id="${m.modelName}Description" class="description">${m.indexdescription}</div>
<h5>${m.credits} </h5>
</div>
  </#if>
</#list>
</div>

</div>
<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.5.1/jquery.min.js"></script>
<script src="code.js"></script>
</body>
</html>