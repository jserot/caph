// Created by iWeb 3.0.4 local-build-20260227

setTransparentGifURL('../Media/transparent.gif');function hostedOnDM()
{return false;}
function photocastSubscribe()
{photocastHelper("http://dream.ispr-ip.fr/CAPH/CAPH/News/rss.xml");}
function onPageLoad()
{loadMozillaCSS('News_files/NewsMoz.css')
adjustLineHeightIfTooBig('id1');adjustFontSizeIfTooBig('id1');adjustLineHeightIfTooBig('id3');adjustFontSizeIfTooBig('id3');detectBrowser();adjustLineHeightIfTooBig('id4');adjustFontSizeIfTooBig('id4');Widget.onload();fixAllIEPNGs('../Media/transparent.gif');fixupIECSS3Opacity('id2');fixupAllIEPNGBGs();performPostEffectsFixups()}
function onPageUnload()
{Widget.onunload();}
