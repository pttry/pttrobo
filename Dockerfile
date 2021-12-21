FROM rocker/tidyverse:4.1.1

# RUN apt-get update \
#     && apt-get install -y --no-install-recommends \
#     nginx \
#     && apt-get clean \
#     && rm -rf /var/lib/apt/lists/* \
#     && rm -rf /tmp/downloaded_packages/ /tmp/*.rds


RUN sed -i -e 's/# fi_FI.UTF-8 UTF-8/fi_FI.UTF-8 UTF-8/' /etc/locale.gen && \
 locale-gen
ENV LANG fi_FI.UTF-8
ENV LANGUAGE fi_FI:en
ENV LC_ALL fi_FI.UTF-8

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN r -e 'devtools::install(upgrade="never")'
WORKDIR /

# RUN echo www-root-path=/rstudio >> /etc/rstudio/rserver.conf
# RUN echo server-health-check-enabled=1 >> /etc/rstudio/rserver.conf
# RUN cat /build_zone/kube/nginx.conf >> /etc/nginx/nginx.conf
# RUN /etc/init.d/nginx restart




