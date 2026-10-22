# A minimal Magento root with a real cache-clean, for the tests in
# magento-cache-clean-docker-test.el, which reach it through Tramp.
FROM php:8.3-cli-bookworm

RUN apt-get update \
 && apt-get install -y --no-install-recommends nodejs procps unzip \
 && rm -rf /var/lib/apt/lists/*

COPY --from=composer:2.8 /usr/bin/composer /usr/bin/composer

WORKDIR /srv/magento

# Only vendor/bin/cache-clean.js, as cache-clean's own requirements.
RUN composer init -n --name test/magento \
 && composer require -n mage-os/magento-cache-clean:1.1.5 \
 && mkdir -p app/etc var/cache var/page_cache generated/code

COPY env.php app/etc/env.php

# A cache-clean on PATH that says so, to tell it from the vendor script.
RUN printf '#!/bin/sh\necho "cache-clean on PATH"\nexec /srv/magento/vendor/bin/cache-clean.js "$@"\n' \
      > /usr/local/bin/cache-clean \
 && chmod +x /usr/local/bin/cache-clean

CMD ["sleep", "infinity"]
