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

backend(tikz);
for(int i = 0; i < 10 ; i += 3) 
{

 doricColumn(i, 0, 9 + (i * 0.1), 0.5, 0.4 + (i * 0.021) , 0.3 + (i * -0.002), 0.3, 1.0);
}

generateTikz("doricColumn",20);