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
 
 
Word[] frankList = new Word[1];
Word[] dracuList = new Word[1];

void setup() {
  size(800, 600);
  background(0);

  // Order the words from Frankenstein and Dracula according to frequency
  frankList = countAndOrderWords("84.txt", frankList);  // Frankenstein
  dracuList = countAndOrderWords("345.txt", dracuList);  // Dracula 

  int numWords = 200;
  int maxSize = 100;
  int wordHeight = 30;
  PFont font = createFont("Monospaced", wordHeight);
  textFont(font);

  float y = maxSize * 0.75;
  int x = 0;
  float maxHeight = 0;
  float ascent = 0;
  float descent = 0;
  
  float exponent = 0.60;

  float nextHeight = maxSize;
  textSize(nextHeight);
  for (int i = 0; i < numWords; i++) {
    if (x == 0) {
      text(frankList[i].ww.toUpperCase(), x, y);
      x += textWidth(frankList[i].ww);
    } else {
      text(frankList[i].ww.toUpperCase(), x, y);
      x += textWidth(frankList[i].ww);
    }

    nextHeight = frankList[i+1].count / float(frankList[0].count);
    nextHeight = pow(nextHeight, exponent);
    nextHeight = nextHeight * maxSize;
    textSize(nextHeight);
    x += textWidth(" ") * 0.75; 
    
    if ((x + textWidth(frankList[i+1].ww + " ")) > width) {
      x = 0; 
      ascent = textAscent();
      descent = textDescent();
      y += ascent + descent * .75;
    }
  }
  
  float ratio = frankList[0].count / float(dracuList[0].count);

  x = 0;
  y = height/2 + maxSize/2;
  
  nextHeight = maxSize;
  textSize(nextHeight);
  
  for (int i = 0; i < numWords; i++) {
    if (x == 0) {
      text(dracuList[i].ww.toUpperCase(), x, y);
      x += textWidth(dracuList[i].ww);
    } else {
      text(dracuList[i].ww.toUpperCase(), x, y);
      x += textWidth(dracuList[i].ww);
    }

    nextHeight = dracuList[i+1].count / float(dracuList[0].count);
    nextHeight = pow(nextHeight, exponent);
    nextHeight = nextHeight * maxSize;
    textSize(nextHeight);
    x += textWidth(" ") * 0.75; 
    
    if ((x + textWidth(dracuList[i+1].ww + " ")) > width) {
      x = 0; 
      ascent = textAscent();
      descent = textDescent();
      y += ascent + descent * .75;
    }
  }

}


Word[] countAndOrderWords(String textFile, Word[] wordList) {

  String[] lines = loadStrings(textFile);
  wordList[0] = new Word("");
  boolean started = false;

  for (int i = 0; i < lines.length; i++) {
    if (lines[i].startsWith("*** START OF")) {  
      started = true;
    } 
    else if (lines[i].startsWith("*** END")) {
      started = false;
    } 
    else if (started == true) {
      String separators = WHITESPACE + ",;.:!?()\"-";
      String[] thisLine = splitTokens(lines[i], separators);

      for (int j = 0; j < thisLine.length; j++) {

        String word = thisLine[j].toLowerCase();

        boolean newWord = true;

        for (int w = 0; w < wordList.length; w++) {
          if (word.equals(wordList[w].ww)) {
            newWord = false;
            wordList[w].count++;
            break;
          }
        }

        if (newWord == true) {
          Word next = new Word(word);
          wordList = (Word[])append(wordList, next);
          wordList[wordList.length-1].wordCount = word.length();
        }

      }
    }
  }

  Arrays.sort(wordList); 
  return wordList;
}
