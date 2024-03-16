// Gheta Andrei-Cristian 312CA
#include <stdio.h>
#include <stdlib.h>
#define ALOCATE 4
#define MOD 10007

// functie care incarca matricea in vectorul de matrice din memorie
int incarca_mat(int ***a, int m, int n)
{
	*a = (int **)malloc(m * sizeof(int *));
	if (!*a)
		return -1;
	for (int i = 0; i < m; i++) {
		(*a)[i] = (int *)malloc(n * sizeof(int));
		if (!(*a)[i]) {
			for (int j = 0; j < i; j++)
				free((*a)[j]);
			free(*a);
			return -1;
		}
	}
	return 0;
}

// functie care realoca numarul de matrice din vectorul de matrice
int realoca_mat(int ****v, int nr_alocate, int k, int *m, int *n)
{
	int ***tmp_v = (int ***)realloc(*v, nr_alocate * sizeof(int **));
	if (!tmp_v) {
		for (int nr = 0; nr < k; nr++) {
			for (int i = 0; i < m[nr]; i++)
				free((*v)[nr][i]);
			free((*v)[nr]);
		}
		free(*v);
		free(m);
		free(n);
		return -1;
	}
	*v = tmp_v;

	return 0;
}

// functie care realoca vectorul ce stocheaza numarul de linii
// al fiecarei matrice din vectorul de matrice
int realoca_lin(int ***v, int nr_alocate, int k, int **m, int *n)
{
	int *tmp = (int *)realloc(*m, nr_alocate * sizeof(int));
	if (!tmp) {
		for (int nr = 0; nr < k; nr++) {
			for (int i = 0; i < (*m)[nr]; i++)
				free(v[nr][i]);
			free(v[nr]);
		}
		free(v);
		free(*m);
		free(n);
		return -1;
	}
	*m = tmp;

	return 0;
}

// functie care realoca vectorul ce stocheaza numarul de coloane
// al fiecarei matrice din vectorul de matrice
int realoca_col(int ***v, int nr_alocate, int k, int *m, int **n)
{
	int *tmp = (int *)realloc(*n, nr_alocate * sizeof(int));
	if (!tmp) {
		for (int nr = 0; nr < k; nr++) {
			for (int i = 0; i < m[nr]; i++)
				free(v[nr][i]);
			free(v[nr]);
		}
		free(v);
		free(m);
		free(*n);
		return -1;
	}
	*n = tmp;

	return 0;
}

// functie de dezalocare a tuturor resurselor alocate pe parcursul programului
void stergere(int ***v, int k, int *m, int *n)
{
	for (int nr = 0; nr < k; nr++) {
		for (int i = 0; i < m[nr]; i++)
			free(v[nr][i]);
		free(v[nr]);
		}
	free(v);
	free(m);
	free(n);
}

// functie de eliminare a unei matrice
void eliminare_mat(int **a, int m)
{
	for (int i = 0; i < m; i++)
		free(a[i]);
	free(a);
}

// functie pentru inmultirea unor matrici patratice
// (de folos pentru ridicarea la putere in timp logaritmic)
void inmultire_mat(int **rezultat, int **mat1, int **mat2, int m)
{
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < m; j++) {
			rezultat[i][j] = 0;
			for (int p = 0; p < m; p++)
				rezultat[i][j] += mat1[i][p] * mat2[p][j] % MOD;
		}
	}
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < m; j++) {
			rezultat[i][j] %= MOD;
			if (rezultat[i][j] < 0)
				rezultat[i][j] += MOD;
		}
	}
}

// functie pentru inmultirea matricelor de orice tip
// (de folos pentru operatia M)
void inmultire_mat2(int **rezultat, int **mat1, int **mat2, int m, int n, int q)
{
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < n; j++) {
			rezultat[i][j] = 0;
			for (int p = 0; p < q; p++)
				rezultat[i][j] += mat1[i][p] * mat2[p][j] % MOD;
		}
	}
	for (int i = 0; i < m; i++) {
		for (int j = 0; j < n; j++) {
			rezultat[i][j] %= MOD;
			if (rezultat[i][j] < 0)
				rezultat[i][j] += MOD;
		}
	}
}

// functia de ridicare la putere a unei matrice in timp logaritmic
int putere_mat(int **a, int putere, int m)
{
	int **p, **p_copie, **a_copie;
	if (incarca_mat(&p, m, m) == -1)
		return -1;
	if (incarca_mat(&p_copie, m, m) == -1) {
		eliminare_mat(p, m);
		return -1;
	}
	if (incarca_mat(&a_copie, m, m) == -1) {
		eliminare_mat(p, m);
		eliminare_mat(p_copie, m);
		return -1;
	}
	for (int i = 0; i < m; i++)
		for (int j = 0; j < m; j++) {
			if (i == j)
				p[i][j] = 1;
			else
				p[i][j] = 0;
		}
	while (putere) {
		if (putere % 2 == 1) {
			// matricea finala se inmulteste cu cea auxiliara
			inmultire_mat(p_copie, p, a, m);
			for (int i = 0; i < m; i++)
				for (int j = 0; j < m; j++)
					p[i][j] = p_copie[i][j];
		}
		// matricea finala devine ea la puterea a 2-a
		inmultire_mat(a_copie, a, a, m);
		for (int i = 0; i < m; i++)
			for (int j = 0; j < m; j++)
				a[i][j] = a_copie[i][j];
		putere /= 2;
	}
	for (int i = 0; i < m; i++)
		for (int j = 0; j < m; j++)
			a[i][j] = p[i][j];
	// se elimina toate matricele auxiliare de care am avut nevoie
	eliminare_mat(p, m);
	eliminare_mat(p_copie, m);
	eliminare_mat(a_copie, m);
	return 0;
}

int operatie_L(int ****v_mat, int **v_lin, int **v_col, int *nr_alocate, int *k)
{
	int m, n;
	scanf(" %d %d", &m, &n);
	if (incarca_mat(&(*v_mat)[*k], m, n) == -1)
		return -1;
	for (int i = 0; i < m; i++)
		for (int j = 0; j < n; j++)
			scanf("%d", &(*v_mat)[*k][i][j]);
	(*v_lin)[*k] = m;
	(*v_col)[*k] = n;
	// de fiecare data verificam daca numarul de matrice incarcate a depasit
	// numarul initial de matrice alocate, in acest caz facem resize la vectori
	if (*k >= *nr_alocate - 1) {
		*nr_alocate *= 2;
		if (realoca_mat(v_mat, *nr_alocate, *k, &m, &n) == -1)
			return -1;
		if (realoca_lin(*v_mat, *nr_alocate, *k, v_lin, *v_col) == -1)
			return -1;
		if (realoca_col(*v_mat, *nr_alocate, *k, *v_lin, v_col) == -1)
			return -1;
	}
	// crestem k-ul de fiecare data ce a fost adaugata o matrice noua in vector
	// pentru a contoriza numarul de matrice pe care le avem incarcate
	*k += 1;
	return 0;
}

int operatie_D(int *v_lin, int *v_col, int k)
{
	int index;
	scanf(" %d", &index);
	if (index >= 0 && index < k)
		printf("%d %d\n", v_lin[index], v_col[index]);
	else
		printf("No matrix with the given index\n");
	return 0;
}

int operatie_P(int ***v_mat, int *v_lin, int *v_col, int k)
{
	int index;
	scanf(" %d", &index);
	if (index >= 0 && index < k) {
		for (int i = 0; i < v_lin[index]; i++) {
			for (int j = 0; j < v_col[index]; j++)
				printf("%d ", v_mat[index][i][j]);
			printf("\n");
		}
	} else {
		printf("No matrix with the given index\n");
	}
	return 0;
}

int operatie_C(int ****v_mat, int **v_lin, int **v_col, int *nr_alocate, int k)
{
	// *linie si *coloana sunt vectorii care retin dupa ce linii si coloane
	// se realizeaza resize-ul
	int index, nr_lin, nr_col, *linie, *coloana;
	scanf(" %d\n", &index);
	scanf("%d\n", &nr_lin);
	if (index >= 0 && index < k) {
		linie = (int *)malloc(nr_lin * sizeof(int));
		if (!linie)
			return -1;
		for (int i = 0; i < nr_lin; i++)
			scanf("%d", &linie[i]);
		scanf("%d\n", &nr_col);
		coloana = (int *)malloc(nr_col * sizeof(int));
		if (!coloana) {
			free(linie);
			return -1;
		}
		for (int i = 0; i < nr_col; i++)
			scanf("%d", &coloana[i]);
		if (k >= *nr_alocate - 1) {
			*nr_alocate *= 2;
			if (realoca_mat(v_mat, *nr_alocate, k, *v_lin, *v_col) == -1)
				return -1;
			if (realoca_lin(*v_mat, *nr_alocate, k, v_lin, *v_col) == -1)
				return -1;
			if (realoca_col(*v_mat, *nr_alocate, k, *v_lin, v_col) == -1)
				return -1;
		}
		if (incarca_mat(&(*v_mat)[k], nr_lin, nr_col) == -1) {
			free(linie);
			free(coloana);
			stergere(*v_mat, k, *v_lin, *v_col);
			return -1;
		}
		(*v_lin)[k] = nr_lin;
		(*v_col)[k] = nr_col;
		for (int i = 0; i < nr_lin; i++)
			for (int j = 0; j < nr_col; j++)
				(*v_mat)[k][i][j] = (*v_mat)[index][linie[i]][coloana[j]];
		eliminare_mat((*v_mat)[index], (*v_lin)[index]);
		if (incarca_mat(&(*v_mat)[index], nr_lin, nr_col) == -1) {
			free(linie);
			free(coloana);
			for (int nr = 0; nr < k; nr++) {
				if (nr != index) {
					for (int i = 0; i < (*v_lin)[nr]; i++)
						free((*v_mat)[k][i]);
					free((*v_mat)[k]);
				}
			}
			free(*v_mat);
			free(*v_lin);
			free(*v_col);
			return -1;
		}
		(*v_lin)[index] = nr_lin;
		(*v_col)[index] = nr_col;
		for (int i = 0; i < nr_lin; i++)
			for (int j = 0; j < nr_col; j++)
				(*v_mat)[index][i][j] = (*v_mat)[k][i][j];
		eliminare_mat((*v_mat)[k], (*v_lin)[k]);
		(*v_lin)[k] = 0;
		(*v_col)[k] = 0;
		free(linie);
		free(coloana);
	} else {
		printf("No matrix with the given index\n");
	}
	return 0;
}

int operatie_M(int ****v_mat, int **v_lin, int **v_col, int *nr_alocate, int *k)
{
	// variabila inmultire ne ajuta sa determinam daca s-a realizat inmultirea
	// ne ajuta la determinarea erorilor care pot aparea
	int index, index2, inmultire = 0;
	scanf(" %d %d", &index, &index2);
	if (!(index >= 0 && index < *k) || !(index2 >= 0 && index2 < *k))
		printf("No matrix with the given index\n");
	if (index >= 0 && index < *k && index2 >= 0 && index2 < *k) {
		if ((*v_col)[index] == (*v_lin)[index2]) {
			inmultire = 1;
			if (*k >= *nr_alocate - 1) {
				*nr_alocate *= 2;
				if (realoca_mat(v_mat, *nr_alocate, *k, *v_lin, *v_col) == -1)
					return -1;
				if (realoca_lin(*v_mat, *nr_alocate, *k, v_lin, *v_col) == -1)
					return -1;
				if (realoca_col(*v_mat, *nr_alocate, *k, *v_lin, v_col) == -1)
					return -1;
			}
			(*v_lin)[*k] = (*v_lin)[index];
			(*v_col)[*k] = (*v_col)[index2];
			incarca_mat(&(*v_mat)[*k], (*v_lin)[*k], (*v_col)[*k]);
			inmultire_mat2((*v_mat)[*k], (*v_mat)[index], (*v_mat)[index2],
						   (*v_lin)[index], (*v_col)[index2], (*v_col)[index]);
			*k += 1;
		}
	}
	if (inmultire == 0 && index >= 0 && index < *k)
		if (index2 >= 0 && index2 < *k)
			printf("Cannot perform matrix multiplication\n");
	return 0;
}

int operatie_T(int ****v_mat, int **v_lin, int **v_col, int *nr_alocate, int k)
{
	int index;
	scanf(" %d", &index);
	if (index >= 0 && index < k) {
		if (k >= *nr_alocate - 1) {
			*nr_alocate *= 2;
			if (realoca_mat(v_mat, *nr_alocate, k, *v_lin, *v_col) == -1)
				return -1;
			if (realoca_lin(*v_mat, *nr_alocate, k, v_lin, *v_col) == -1)
				return -1;
			if (realoca_col(*v_mat, *nr_alocate, k, *v_lin, v_col) == -1)
				return -1;
		}
		// incarcam o matrice "pe dos", cu numarul de linii egal cu numarul
		// de coloane si invers pentru a forma transpusa
		if (incarca_mat(&(*v_mat)[k], (*v_col)[index], (*v_lin)[index]) == -1) {
			stergere(*v_mat, k, *v_lin, *v_col);
			return -1;
		}
		(*v_lin)[k] = (*v_col)[index];
		(*v_col)[k] = (*v_lin)[index];
		for (int i = 0; i < (*v_lin)[k]; i++)
			for (int j = 0; j < (*v_col)[k]; j++)
				(*v_mat)[k][i][j] = (*v_mat)[index][j][i];
		eliminare_mat((*v_mat)[index], (*v_lin)[index]);
		if (incarca_mat(&(*v_mat)[index], (*v_lin)[k], (*v_col)[k]) == -1) {
			for (int nr = 0; nr < k; nr++) {
				if (nr != index) {
					for (int i = 0; i < (*v_lin)[nr]; i++)
						free((*v_mat)[nr][i]);
					free((*v_mat)[nr]);
				}
			}
			free(*v_mat);
			free(*v_lin);
			free(*v_col);
			return -1;
		}
		(*v_lin)[index] = (*v_lin)[k];
		(*v_col)[index] = (*v_col)[k];
		for (int i = 0; i < (*v_lin)[index]; i++)
			for (int j = 0; j < (*v_col)[index]; j++)
				(*v_mat)[index][i][j] = (*v_mat)[k][i][j];
		eliminare_mat((*v_mat)[k], (*v_lin)[k]);
		(*v_lin)[k] = 0;
		(*v_col)[k] = 0;
	} else {
		printf("No matrix with the given index\n");
	}
	return 0;
}

int operatie_O(int ****v_mat, int **v_lin, int **v_col, int k)
{
	// alocam dinamic vectorul *suma care va retine suma elementelor
	// fiecarei matrice
	int *suma;
	suma = (int *)calloc(k, sizeof(int));
	if (!suma)
		return -1;
	for (int i = 0; i < k; i++) {
		for (int j = 0; j < (*v_lin)[i]; j++)
			for (int p = 0; p < (*v_col)[i]; p++)
				suma[i] = (suma[i] + (*v_mat)[i][j][p]) % MOD;
		if (suma[i] < 0)
			suma[i] += MOD;
	}
	for (int i = 0; i < k - 1; i++)
		for (int j = i + 1; j < k; j++) {
			if (suma[i] > suma[j]) {
				int **v_aux = (*v_mat)[i];
				(*v_mat)[i] = (*v_mat)[j];
				(*v_mat)[j] = v_aux;
				int v_aux_linii = (*v_lin)[i];
				(*v_lin)[i] = (*v_lin)[j];
				(*v_lin)[j] = v_aux_linii;
				int v_aux_coloane = (*v_col)[i];
				(*v_col)[i] = (*v_col)[j];
				(*v_col)[j] = v_aux_coloane;
				int suma_aux = suma[i];
				suma[i] = suma[j];
				suma[j] = suma_aux;
			}
		}
	free(suma);
	return 0;
}

int operatie_R(int ****v_mat, int **v_lin, int **v_col, int k)
{
	int index, putere;
	scanf(" %d %d", &index, &putere);
	if (index >= 0 && index < k && putere > 0) {
		if ((*v_lin)[index] == (*v_col)[index]) {
			if (putere_mat((*v_mat)[index], putere, (*v_lin)[index]) == -1) {
				stergere(*v_mat, k, *v_lin, *v_col);
				return -1;
			}
		} else {
			printf("Cannot perform matrix multiplication\n");
		}
	} else if (!(index > 0 && index < k) && putere > 0) {
		printf("No matrix with the given index\n");
	} else if (index > 0 && index < k && putere < 0) {
		printf("Power should be positive\n");
	} else if (!(index > 0 && index < k) && putere < 0) {
		printf("No matrix with the given index\n");
		printf("Power should be positive\n");
	}
	return 0;
}

int operatie_F(int ****v_mat, int **v_lin, int **v_col, int *nr_alocate, int *k)
{
	int index;
	scanf(" %d", &index);
	if (index >= 0 && index < *k) {
		eliminare_mat((*v_mat)[index], (*v_lin)[index]);
		// mutam elementele din matrice incepand de la index + 1 spre stanga
		for (int i = index + 1; i < *k; i++) {
			(*v_mat)[i - 1] = (*v_mat)[i];
			(*v_lin)[i - 1] = (*v_lin)[i];
			(*v_col)[i - 1] = (*v_col)[i];
		}
		// ultimul element din vectorul de matrice va pointa spre acelasi lucru
		// ca penultimul asa ca ii atribuim valoarea NULL ultimului
		(*v_mat)[*k - 1] = NULL;
		(*v_lin)[*k - 1] = 0;
		(*v_col)[*k - 1] = 0;
		// scadem contorul care ne indica numarul de matrice din vector
		*k -= 1;
		// daca au fost sterse mai mult de jumate din matrice, se face resize
		if (*k < *nr_alocate / 2) {
			*nr_alocate /= 2;
			if (realoca_mat(v_mat, *nr_alocate, *k, *v_lin, *v_col) == -1)
				return -1;
			if (realoca_lin(*v_mat, *nr_alocate, *k, v_lin, *v_col) == -1)
				return -1;
			if (realoca_col(*v_mat, *nr_alocate, *k, *v_lin, v_col) == -1)
				return -1;
		}
	} else {
		printf("No matrix with the given index\n");
	}
	return 0;
}

int main(void)
{
	int ***v_mat, *v_lin, *v_col, k = 0, nr_alocate = ALOCATE;
	char operatie;
	// alocam initial vectorii cu o dimensiune ALOCATE
	// in caz ca esueaza, dezalocam tot
	v_mat = (int ***)malloc(nr_alocate * sizeof(int **));
	if (!v_mat)
		return -1;
	v_col = (int *)malloc(nr_alocate * sizeof(int));
	if (!v_col) {
		free(v_mat);
		return -1;
	}
	v_lin = (int *)malloc(nr_alocate * sizeof(int));
	if (!v_lin) {
		free(v_col);
		free(v_mat);
		return -1;
	}
	scanf("%c", &operatie);
	while (operatie != 'Q') {
		switch (operatie) {
		case 'L':
			if (operatie_L(&v_mat, &v_lin, &v_col, &nr_alocate, &k) == -1)
				return -1;
			break;
		case 'D':
			operatie_D(v_lin, v_col, k);
			break;
		case 'P':
			operatie_P(v_mat, v_lin, v_col, k);
			break;
		case 'C':
			if (operatie_C(&v_mat, &v_lin, &v_col, &nr_alocate, k) == -1)
				return -1;
			break;
		case 'M':
			if (operatie_M(&v_mat, &v_lin, &v_col, &nr_alocate, &k) == -1)
				return -1;
			break;
		case 'T':
			if (operatie_T(&v_mat, &v_lin, &v_col, &nr_alocate, k) == -1)
				return -1;
			break;
		case 'O':
			if (operatie_O(&v_mat, &v_lin, &v_col, k) == -1)
				return -1;
			break;
		case 'R':
			if (operatie_R(&v_mat, &v_lin, &v_col, k) == -1)
				return -1;
			break;
		case 'F':
			if (operatie_F(&v_mat, &v_lin, &v_col, &nr_alocate, &k) == -1)
				return -1;
			break;
		// cazuri de exceptat la care ignoram si dam break;
		case '\n':
		case ' ':
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			break;
		default:
			printf("Unrecognized command\n");
			break;
		}
		scanf("%c", &operatie);
	}
	if (operatie == 'Q')
		stergere(v_mat, k, v_lin, v_col);

	return 0;
}
