/*
 * 443 String Compression
 */

def compress(chars: Array[Char]): Int = {

  var chix = 0
  var nix = 0
  var i = 1

  while (i < chars.length) {
    if (chars(i) == chars(chix)) {
      chix = i
      nix = chix - 1
      chars(nix) = 2
    } else {
      
    }

    i += 1
  }
}