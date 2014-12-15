#lang processing
/**
 * Visualize: Loading and Displaying Data
 * from Form+Code in Design, Art, and Architecture 
 * by Casey Reas, Chandler McWilliams, and LUST
 * Princeton Architectural Press, 2010
 * ISBN 9781568989372
 * 
 * This code was written for Processing 1.2+
 * Get Processing at http://www.processing.org/download
 */

// ---------------
// Word.pde
// ---------------
class Word implements Comparable {

  String ww;
  int count = 1;
  int wordCount = 0;

  Word(String txt) {
    ww = txt;
  }

  // If we want to sort based on the count value of Word:
  int compareTo(Object o)
  {
    Word other = (Word)o;
    if(other.count > count) {  
      return 1;
    }
    if(other.count == count) {
      return 0;
    } else {
      return -1;
    }
  }
}
