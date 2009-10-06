HN.Util.max_id = -1;

HN.Util.find_max = function() {
  var max;
  var tags;
  var i;
  var bits;
  max = 0;
  tags = document.getElementsByTagName("*");
  for (i = 1; i < tags.length; i += 1) {
    if (tags[i].id) {
      bits=tags[i].id.split("_");
      if (bits[0] === "id") {
        if (bits[1] && parseInt(bits[1]) > max) {
          max = parseInt(bits[1]);
        }
      }
    }
  }
  HN.Util.max_id = max + 1;
};

HN.Util.get_new_id = function()
{
  HN.Util.max_id += 1;
  return "id_" + HN.Util.max_id;
};
