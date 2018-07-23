package edu.upc.modelvsdocument;

import java.util.Arrays;
import java.util.LinkedHashSet;
import java.util.Set;

/**Aquesta classe resol un LAP i quantifica la solucio.
 * @author Gerard Nu�ez Ruiz
 */

public class LAPSolver {
	
	/**Aquesta assignacio es una solucio que indica per cada planeta el paquet que li correspon*/
	private int[] assignacio; 
	
	/**La puntuacio es el sumatori dels costos de l'assignacio que resol el LAP*/
	private float puntuacio; 
	
	/**Constructora buida: crea una nova instancia de LAPSolver,
	 * a mes, inicialitza la puntuacio a zero.*/
	public LAPSolver(int n) {
		assignacio = new int[n];
		puntuacio = 0;
	}

    public void setSize (int n) {
        if (assignacio.length != n) { 
            assignacio = new int[n];
        }
    }

	
	/**@pre s'ha cridat el metode resol abans d'aquet. Altrament l'assignacio sera buida.
	 * @return un vector que correspon a l'assignacio que resol el LAP*/
	public int[] getAssignacio() {return assignacio;}
	
	/**@pre s'ha cridat el metode resol abans d'aquest. Altrament la puntuacio sera zero.
	 * @return la puntuacio associada a l'assignacio que resl el LAP*/
	public float getPuntuacio() {return puntuacio;}

	/**@param F es una Matriu de costos entres planetas i paquets.
	 * Calcula una assignacio que resol el LAP i calcula la puntuacio d'aquesta assignacio.*/
	public void resol(Matriu F) {
		assignacio = hungarian(F);
		
		//un cop tenim l'assignacio calculem la seva puntuacio:
		for(int i = 0; i < assignacio.length; ++i) {
			puntuacio = puntuacio + F.accedir(i, assignacio[i]);
		}
	}
	
	/**Metode que calcula l'assignacio donada una Matriu.
	 * @param F es una Matriu de costos
	 * @return Una vector d'enters que correspon a l'assignacio entre files i columnes.*/
	private int[] hungarian(Matriu F) {
			int[] assig = new int[F.getSize()];
			
			/*vectors auxiliars:*/
			int[] zerosFila = new int[F.getSize()]; Arrays.fill(zerosFila, -1);
			int[] zerosCol = new int[F.getSize()]; Arrays.fill(zerosCol, -1);
			int[] zerosPrincipalsFila = new int[F.getSize()]; Arrays.fill(zerosPrincipalsFila, -1);
			
			boolean[] filesCobertes = new boolean[F.getSize()];
			boolean[] colCobertes = new boolean[F.getSize()];
			
			int[] zeroPrincipal = new int[2];
			
			Matriu costMin = new Matriu(F.getSize());
			costMin.copia(F);
			
			// Constrium la matriu de costos minims calculant zeros.
			redueix(costMin);
			
			//Un cop creada la matriu de costos minims calculem si es optima, en cas contrari hem de cobrirlos tot i si cal fer mes zeros.
			buscaZeros(costMin, zerosFila, zerosCol);
			int ncol = cobreixColumnes(zerosCol, colCobertes);

			//en aquest punt si totes les columnes estan cobertes hem acabat, en cas contrari hem de fer mes zeros). 
			while(ncol < costMin.getSize()) {
				
				while(!hihaNousZerosPrincipals(costMin, zerosPrincipalsFila, filesCobertes, colCobertes, zeroPrincipal)) {
					//si no hi ha nous zeros principals hem de fer mes.
					fesMesZeros(costMin, filesCobertes, colCobertes);
				}
				
				int indexCol = zerosFila[zeroPrincipal[0]];

				if(indexCol != -1) {
					//si el zero principal ja te una columna selecionada cobrim la fila del nou zero principal i descobrim la columna de la mateixa fila.
					filesCobertes[zeroPrincipal[0]] = true;
					colCobertes[indexCol] = false;
				}
				else {
					//si el zero principal no ha estat assignat incrementem els zeros i tornem a cobrir.
					assignaZeros(zeroPrincipal, zerosFila, zerosCol, zerosPrincipalsFila);
					Arrays.fill(zerosPrincipalsFila, -1);
					Arrays.fill(filesCobertes, false); Arrays.fill(colCobertes, false); //Tornem a comen�ar amb el nou zero principal.
					ncol = cobreixColumnes(zerosCol, colCobertes);
				}
				
			}
			
			//ara sabem una assignacio optima.
			for(int i = 0; i < zerosCol.length; ++i) {
				assig[zerosCol[i]] = i;
			}
			
			return assig;
			
		}
	
	/**Crea zeros en la Matriu del parametre restant el minim de files i columnes.
	 * @param A es una Matriu*/
	private void redueix(Matriu A) {
		// filas:
		for (int i = 0; i < A.getSize(); ++i) {
			float min = A.getMinFila(i);
			A.restarFila(min, i);
		}
		// columnas:
		for (int j = 0; j < A.getSize(); ++j) {
			if (!A.columZero(j)) { // si no hi ha cap zero en la columna s'ha de reduir.
				float min = A.getMinColumna(j);
				A.restarColumna(min, j);
			}
		}
	}
	
	/**Busca zeros en files i columnes i els selecciona de manera unica.
	 * @param A es la Matriu que conte zeros
	 * @param zerosFila de enters es un vector que conte per cada fila en quina columna esta el zero
	 * @param zerosCol de enters es un vector que conte per cada columna en quina fila esta el zero.*/
	private void buscaZeros(Matriu A, int[] zerosFila, int[] zerosCol) {
		boolean[] filaAmbZero = new boolean[A.getSize()];
		boolean[] colAmbZero = new boolean[A.getSize()];
		
		for (int i = 0; i < A.getSize(); ++i) {
			for (int j = 0; j < A.getSize(); ++j) {
				if(A.accedir(i,j) == 0 && !filaAmbZero[i] && !colAmbZero[j]) {
					zerosFila[i] = j;
					zerosCol[j] = i;
					filaAmbZero[i] = true; //marquem la fila com seleccionada
					colAmbZero[j] = true; //marquem la columna com seleccionada
					break; //pasem a una altre fila.
				}
			}
		}
	}
	
	/**Segons els zeros seleccionats en cada columna es cobreixen els zeros per comprobar si podem trobar una solucio optima
	 * @param zerosCol es un vector de enters que conte per cada columna en quina fila esta el zero
	 * @param colcover es un vector de booleans que indiquen si la columna esta coberta o no
	 * @return un enter que es el nombre de columnes cobertes que estara entre 0 i el tamany de colcover.*/
	private int cobreixColumnes(int[] zerosCol, boolean[] colcover) {
		int colCobertes = 0;
		for(int i = 0; i < zerosCol.length; ++i) {
			if(zerosCol[i] == -1) colcover[i] = false; //si no tenim un zero en aquella columna no la cobrim.
			else { //altrament, la cobrim i augmentem el nombre de columnes cobertes.
				colcover[i] = true;
				++colCobertes;
			}
		}
		
		return colCobertes;
	}
	
	/**Busca un zero principal en les files i columnes que encara no s'han cobert i per tant no s'han assignat.
	 * @param A es una Matriu de costos reduida amb zeros
	 * @param zerosPrncipalsFila es un vector de enters que per cada fila indica en quina columna esta el zero
	 * @param filaCover es un vector de booleans que indiquen si la fila esta coberta o no
	 * @param colCover es un vector de booleans que indiquen si la columna esta coberta o no
	 * @param zeroPrincipal es un vector de enters de dues posicions que indica la posicio del nou zero principal trobat
	 * @return retorna true si <i>zeroPrincipal</i> no es buit o a canviat respecte el valor anterior, altrament retorna false*/
	private boolean hihaNousZerosPrincipals(Matriu A, int[] zerosPrincipalsFila, 
											boolean[] filaCover, boolean[] colCover, int[] zeroPrincipal) {
		for(int i = 0; i < A.getSize(); ++i) {
			if(!filaCover[i]) { //si la fila no esta coberta pasem a buscar una columna.
				
				for(int j = 0; j < A.getSize(); ++j) {
					if(A.accedir(i,j) == 0 && !colCover[j]) {
						//si hi ha aun zero a la matriu de costos i no esta coberta la columna, l'actualitzem com un nou zero principal i retornem cert.
						zerosPrincipalsFila[i] = j;
						
						zeroPrincipal[0] = i; zeroPrincipal[1] = j; //aquesta es l'assignacio del zero principal nou.
						
						return true;
					}
				}
			}
		}
		return false;
	}
	
	/**Donat que el nombre de columnes cobertes es menor a la mida de columnes
	 * restem el minim del valors sense cobrir a les columnes no cobertes
	 * i el sumem a les files cobertes.
	 * @param A es una Matriu de costos reduida amb zeros
	 * @param filaCover es un vector de booleans que indiquen si la fila esta coberta o no
	 * @param colCover es un vector de booleans que indiquen si la columna esta coberta o no.*/
	private void fesMesZeros(Matriu A, boolean[] filaCover, boolean[] colCover) {
		float minSenseCobrir = Float.MAX_VALUE;
		
		for(int i = 0; i < A.getSize(); ++i) {
			if(!filaCover[i]) {
				for(int j = 0; j < A.getSize(); ++j) {
					if(!colCover[j] && (A.accedir(i,j) < minSenseCobrir)) {
						minSenseCobrir = A.accedir(i,j);
					}
				}
			}
		}
		
		//un cop trobat el valor minim, el restem a tots els valors de les columnes no cobertes i sumem el valor minim a les files cobertes:
		for(int i = 0; i < colCover.length; ++i) {
			if(!colCover[i]) {
				for(int j = 0; j < A.getSize(); ++j) {
					A.restar(j,i, minSenseCobrir);
				}
			}
			if(filaCover[i]) {
				for(int k = 0; k < A.getSize(); ++k) {
					A.sumar(i,k, minSenseCobrir);
				}
			}
		}
	}
	
	/**Donat un zero principal i sabent els zeros per files, columnes, i els principals per files
	 * assigna els nous zeros tan per fila com per columna
	 * @param zeroPrincipal es un vector de enters de dues posicions que indica la posicio del nou zero principal trobat
	 * @param zerosFila de enters es un vector que conte per cada fila en quina columna esta el zero
	 * @param zerosCol de enters es un vector que conte per cada columna en quina fila esta el zero
	 * @param zerosPrncipalsFila es un vector de enters que per cada fila indica en quina columna esta el zero.*/
	private void assignaZeros(int[] zerosPrincipals, int[] zerosFila, int[] zerosCol, int[] zerosPrincipalsFila) {
		int i,j;
		j = zerosPrincipals[1];
		
		Set<int[]> sequenciaZeros = new LinkedHashSet<int[]>();
		sequenciaZeros.add(zerosPrincipals);
		
		boolean parella = false;
		
		do {
			i = zerosCol[j];
			int[] newseq = new int[2];
			newseq[0] = i; newseq[1] = j;
			parella = ((i != -1) && sequenciaZeros.add(newseq));
			if(!parella) break;
			
			j = zerosPrincipalsFila[i];
			int[] newseq2 = new int[2];
			newseq2[0] = i; newseq2[1] = j;
			parella = ((j != -1) && sequenciaZeros.add(newseq2));
			
		} while(parella);
		
		for(int[] zero : sequenciaZeros) {
			//Si es un zero principal el seleccionem a la fila i la columna.
			if(zerosPrincipalsFila[zero[0]] == zero[1]) {
				zerosFila[zero[0]] = zero[1];
				zerosCol[zero[1]] = zero[0];
			}
		}
		
	}
	
}
