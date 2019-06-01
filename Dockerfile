FROM trestletech/plumber

RUN Rscript -e "install.packages('e1071')"
RUN Rscript -e "install.packages('caret')"

COPY modelos /app/modelos/
COPY Plumber.R /app

WORKDIR /app

CMD ["Plumber.R"]

EXPOSE 8000
