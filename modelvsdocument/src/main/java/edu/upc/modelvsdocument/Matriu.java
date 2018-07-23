package edu.upc.modelvsdocument;

import java.io.Serializable;

public class Matriu implements Serializable {
	private static final long serialVersionUID = 26027L;
	private float matriu[][];
    int n;
    
    public Matriu (int n){
    //pre: El valor de n >= 0.
    //post: Crea una matriu de dimensi� nxn amb tot de zeros.
       matriu = new float[n][n];
       this.n = n;
       for(int i= 0; i < n; ++i){
           for(int j=0; j < n;++j){
               matriu[i][j] = 0;
           }
       }
    }

    public Matriu (float[][] matriu) {
    //pre: matriu és quadrada
    //post: Crea una matriu fent servir "matriu"
        n = matriu.length;
        this.matriu = matriu;
    }
    
    public void copia(Matriu A) {
    //pre: La matriu A no est� buida.
    //post: El par�metre impl�cit es una c�pia de la matriu A.
        n = A.getSize();
        matriu = new float[n][n];
        int i,j;
        for(i=0; i < n; ++i) {
            for(j=0; j < n; ++j) {
                matriu[i][j] = A.accedir(i,j);
            }
        }
    }

    
    public boolean filaZero(int fila) {
    //pre: 0<=fila<nfiles
    //post: Retorna true si hi ha algun zero a la fila 'fila'.
        boolean zero = false;
        int k = 0;
        while(!zero && k<n) {
          zero=(matriu[fila][k]==0);
          ++k;
        }
        return zero;
    }
    
    public boolean columZero(int columna) {
    //pre: 0<=columna<ncolumnes
    //post: Retorna true si hi ha algun zero a la columna 'columna'.
        boolean zero = false;
        int k = 0;
        while(!zero && k<n) {
          zero=(matriu[k][columna]==0);
          ++k;
        }
        return zero;
    }
    
    public void asigna(int i, int j, float valor){
    //pre: 0 <= i,j < n .
    //post: la posici� [i][j] de la matriu cont� el float valor
           matriu[i][j] = valor;
    }
    
    public int getSize(){
    //pre: La matriu impl�cita no est� buida.
    //post: retorna el tamany de la matriu.
         return n;
    }
    
  
    public void multiplica (Matriu A, Matriu B) {
    //pre: Les matrius tenen el mateix tamany.
    //post: Retorna la matriu resultant del producte de les matrius A i B.
        int i,j,k;
        for(i = 0; i < n; i++){
            for (j = 0; j < n; j++){
                float sum = 0;
                for (k = 0; k < n; k++){
                    sum = sum + A.accedir(i,k)*B.accedir(k,j);
                }
                matriu[i][j] = sum;
            }
        }
    }
    
    public void suma (Matriu A){
     //pre: Les matrius tenen el mateix tamany.
    //post: el parametre impl�cit �s la matriu resultant de la suma de matrius A i B;
        for(int i = 0; i < n; ++i ){
            for(int j = 0; j < n; ++j){
               matriu[i][j] += A.accedir(i,j);
            }
        }
         
    }
    
    public void multiplicaFloat(float k){
    //pre: El par�metre impl�cit no �s buit.
    //post: Els valors de la matriu impl�cita s�han multiplicat per k.
        for(int i = 0; i < n; ++i ){
            for(int j = 0; j < n; ++j){
                matriu[i][j] *= k;
            }
        }
    }
    public float accedir (int i, int j){
    //pre: El tamany de la matriu es superior als parametres i, j.
    //post: Retorna el valor que es troba a la fila i, columna j.
        return matriu[i][j];
    }
    
    public void sumar(int i, int j, float valor) {
    //pre: Cert
    //post: S'ha sumat 'valor' al valor de la posici� i,j de la matriu.
        matriu[i][j] += valor;
    }
    
    public void restar(int i, int j, float valor) {
    //pre: Cert
    //post: S'ha restat 'valor' al valor de la posici� i,j de la matriu.
        matriu[i][j] -= valor;
    }

    public void restarColumna(float k,int columna){
    //pre: El par�metre impl�cit  no �s buit.
    //post: Resta el valor �k� a la columna 'columna' del par�metre impl�cit.
        int i;
        for(i = 0; i < n; ++i) {
            matriu[i][columna] -= k;
        }
    }
    
    public void restarFila(float k,int fila){
    //pre: El par�metre impl�cit  no �s buit.
    //post: Resta el valor �k� a la fila �fila� del par�metre impl�cit.
        int i;
        for(i = 0; i < n; ++i) {
            matriu[fila][i] -= k;
        }
    }
    
    public float getMinColumna(int col){
    //pre: La matriu A no és buida.
    //post: Retorna el valor de l’element mínim de la columna 'columna'.
        float min=matriu[0][col];
        for(int fila=1; fila < this.n; ++fila) { // el this. es redundant en aquest cas.
            if(matriu[fila][col]<min) min = matriu[fila][col];
        }
        return min;
    }

    public float getMinFila(int fila){
    //pre: La matriu A no és buida.
    //post: Retorna el valor de l’element mínim de la fila ‘fila’.
        float min=matriu[fila][0];
        for(int col=1; col < this.n; ++col) { // el this. es redundant en aquest cas.
            if(matriu[fila][col]<min) min = matriu[fila][col];
        }
        return min;
    }
    
    public float getMaxFila(int fila){
    //pre: La matriu A no és buida.
    //post: Retorna el valor de l’element mínim de la fila ‘fila’.
        float max=matriu[fila][0];
        for(int col=1; col < this.n; ++col) { // el this. es redundant en aquest cas.
            if(matriu[fila][col]>max) max = matriu[fila][col];
        }
        return max;
    }
    
    public float getMaxMatriu () {
    //pre: La matriu impl�cita no �s buida. 
    //post: Retorna l'element m�xim de la matriu implicita.
        int i;
        float max = matriu[0][0];
        for(i = 0; i < this.n; ++i) {
            float max2;
            max2 = getMaxFila(i);
            if(max2 > max) max = max2;
        }
        return max;
    }
    
    public int numZerosFila(int fila) {
    //pre: La matriu impl�cita no est� buida i fila>=0 & fila<n.
    //post: Retorna el n�mero de zeros que hi han a la fila 'fila' de la matriu impl�cita.
        int i;
        int zeros = 0;
        for(i = 0; i < n; ++i) {
            if(matriu[fila][i] == 0) ++zeros;
        }
        return zeros;
    }
    
    public int numZerosColumna(int columna) {
    //pre: La matriu impl�cita no est� buida i fila>=0 & fila<n.
    //post: Retorna el n�mero de zeros que hi han a la columna 'columna' de la matriu impl�cita.
        int i;
        int zeros = 0;
        for(i = 0; i < n; ++i) {
            if(matriu[i][columna] == 0) ++zeros;
        }
        return zeros;
    }
    
    public void Swap(int i, int j, int k, int l){
     //pre: 0 <= i,j,k,l < n.
     //post: intercanvia les posicions [i][j] i [k][l] de la matriu impl�cita.
        float aux = matriu[i][j];
        asigna(i,j, accedir(k,l));
        asigna(k,l, aux);
    }

    public String toString() {
        String m = "";
        for(int i = 0; i < matriu.length; ++i) {
            for (int j = 0; j < matriu[0].length; ++j) {
                m += Float.toString(matriu[i][j]) + ", ";
            }
            m += "\n";
        }
        return m;
    }

}
