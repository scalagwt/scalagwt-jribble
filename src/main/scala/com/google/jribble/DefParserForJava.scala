package com.google.jribble

/**
 * Class needed just because there is no way to use
 * mix-in composition in Java.
 */
class DefParserForJava extends DefParser with CachingDefParser