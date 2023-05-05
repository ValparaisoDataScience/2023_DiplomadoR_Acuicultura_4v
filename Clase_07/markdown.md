# Hoja de referencia de Markdown

Esta hoja de referencia fue elaborada por el **Dr. José Gallardo Matus** en abril del 2023.

## Objetivo de aprendizaje

Esta hoja de referencia está elaborada con Markdown y tiene como propósito ser utilizada como una actividad de aprendizaje de la sintaxis de Markdown a un nivel inicial. Por tanto, no cubre todas las potencializades del software.  

## Sintaxis Básica

Estos son los elementos básicos que debes conocer para elaborar cualquier documento Markdown.

### Encabezados de distintos tamaños

# H1
## H2
### H3

### Texto en negrita

**Texto en negrita**

### Texto en cursiva

*Texto en cursiva*

### Citar en un bloque de texto

> Recordar un buen momento es sentirse feliz de nuevo - Gabriela Mistral

### Lista ordenada de elementos

1. Arancibia
2. Gallardo
3. Rueda

### Lista no ordenada

- Phyton
- R
- Matlab

### Códigos de programación

Opción 1: Codigos entre tildes invertidas.

`mean(20,25)`

Opción 2: Tabular 2 veces.

    mean(20,25)

### Línea horizontal

---

### Enlace web

[Valparaiso ciencia de datos](https://github.com/ValparaisoDataScience)

## Extended Syntax

These elements extend the basic syntax by adding additional features. Not all Markdown applications support these elements.

### Tablas justificadas

| Año | Mes |
| :----------- | :----------- |
| 2013 | Enero |
| 2013 | Febrero |

| Año | Mes |
| -----------: | -----------: |
| 2013 | Enero |
| 2013 | Febrero |

| Año | Mes |
| :-----------: | :-----------: |
| 2013 | Enero |
| 2013 | Febrero |

### Nota a pie de página

Recordar un buen momento es sentirse feliz de nuevo. [^1]

[^1]: Por Gabriela Mistral.

### Lista de verificación

- [x] Preparar la clase de Diplomado
- [X] Subir la clase al Drive
- [] Tomar café antes de la clase.

### Expresiones matemáticas

La expresiones matemáticas deben señalarse con el simbolo **$** antes y después.

| Expresión matemática | Documento|
| :-----------: | :-----------: | :-----------: |
| Raíz cuadrada	 | $\sqrt{x}$ |
| Fracciones |  $\frac{a}{b+c}$ |
| Potencia | $r^2$ o $2^{24}$ |
| Mas menos | $25 \pm 3.0$|
| Aproximado  | $peso \approx 3.5 k$ |
| Distinto | $peso \neq 3.5$ |
| Sub índice | $O_2$ |
| Sumatoria | $\sum_{n=1}^{10}n$ |

La expresiones matemáticas también pueden señalarse con begin{equation} y end{equation}.

La correlación entre las variables fue: 
\begin{equation}
r^2=0.89
\end{equation}

## Sitios web recomendados

Para conocer al autor de Markdown [Daring Fireball ](https://daringfireball.net/projects/markdown/)!

Para visualizar el resultado de sintaxis Markdown en línea [Markdown live preview ](https://markdownlivepreview.com) por [Hideaki Tanabe](https://github.com/tanabe).

Para generar tablas en markdown [Generador de tablas markdwon](https://www.tablesgenerator.com/markdown_tables)

Para aprender sintaxis avanzada de Markdown [Mardown Guide](https://www.markdownguide.org/extended-syntax).

Para aprender sintaxis de expresiones matemáticas, Markdown permite usar [Latex](https://manualdelatex.com/tutoriales/ecuaciones)





