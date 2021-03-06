% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/steps_states.R
\name{Graph}
\alias{Graph}
\title{Graph class}
\description{
Graph class

Graph class
}
\section{Super class}{
\code{\link[stepfunctions:Block]{stepfunctions::Block}} -> \code{Graph}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{Graph$new()}}
\item \href{#method-allowed_fields}{\code{Graph$allowed_fields()}}
\item \href{#method-contains}{\code{Graph$contains()}}
\item \href{#method-build_graph}{\code{Graph$build_graph()}}
\item \href{#method-to_list}{\code{Graph$to_list()}}
\item \href{#method-clone}{\code{Graph$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="stepfunctions" data-topic="Block" data-id="format">}\href{../../stepfunctions/html/Block.html#method-format}{\code{stepfunctions::Block$format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="stepfunctions" data-topic="Block" data-id="is_field_allowed">}\href{../../stepfunctions/html/Block.html#method-is_field_allowed}{\code{stepfunctions::Block$is_field_allowed()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="stepfunctions" data-topic="Block" data-id="to_json">}\href{../../stepfunctions/html/Block.html#method-to_json}{\code{stepfunctions::Block$to_json()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize Graph class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Graph$new(branch, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{branch}}{(State, Chain):}

\item{\code{...}}{: Extra field names to pass to Block class}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-allowed_fields"></a>}}
\if{latex}{\out{\hypertarget{method-allowed_fields}{}}}
\subsection{Method \code{allowed_fields()}}{
allowed extra fields
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Graph$allowed_fields()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-contains"></a>}}
\if{latex}{\out{\hypertarget{method-contains}{}}}
\subsection{Method \code{contains()}}{
check if state is contained in class
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Graph$contains(state)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{state}}{:}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-build_graph"></a>}}
\if{latex}{\out{\hypertarget{method-build_graph}{}}}
\subsection{Method \code{build_graph()}}{
Create Graph
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Graph$build_graph(state)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{state}}{:}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-to_list"></a>}}
\if{latex}{\out{\hypertarget{method-to_list}{}}}
\subsection{Method \code{to_list()}}{
Convert class to list ready to be translated for
             Amazon States Language \url{https://states-language.net/spec.html}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Graph$to_list()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{Graph$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
