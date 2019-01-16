var el = document.getElementsByClassName('pl-video-edit-remove');
var elt = document.getElementsByClassName('pl-video');
var index = 0;

function deleteVideo() {
  if (index < el.length) {
    if (elt[index].attributes[3].value == '[Deleted video]' ||
        elt[index].attributes[3].value == '[Private video]'){
      el[index].click();
    }
    index++;
    setTimeout(deleteVideo, 300);
  }
}
deleteVideo();