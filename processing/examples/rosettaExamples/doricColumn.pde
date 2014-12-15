#lang processing

void drawFuste(float x, float y, float fuste, float base, float top)
{
 quad(x + (-top), y + fuste, 
      x + (-base), y,
      x + base, y,
      x + top, y + fuste);
}

void drawCoxim(float x, float y, float coxim, float base, float top)
{
 quad(x - base, y, 
      x - top, y + coxim,
      x + top, y + coxim,
      x + base, y);
}
                                                                 
void drawAbaco(float x, float y, float aAbaco, float lAbaco)
{
 quad( x , y, 
       x, y + aAbaco,
       x + lAbaco, y + aAbaco,
       x + lAbaco, y);
}


void doricColumn(float x, float y, float fuste, float baseFuste, float coxim, float baseCoxim, float aAbaco, float lAbaco)
{       
  drawFuste( x, y, fuste, baseFuste, baseCoxim);

  drawCoxim( x, y + fuste, coxim, baseCoxim, lAbaco / 2);
  
  drawAbaco( x - lAbaco/2, y + (fuste + coxim), aAbaco, lAbaco);
}

void setup(){
    backend(tikz);
    doricColumn(0, 0, 9, 0.5, 0.4, 0.3, 0.3, 1.0);
    generateTikz("doricColumn");
}
