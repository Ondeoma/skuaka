# skuaka アプリ（scripts/dist.sh の成果物を詰めるだけ）
FROM eclipse-temurin:25-jre-alpine
RUN addgroup -S skuaka && adduser -S -G skuaka skuaka
COPY dist/lib /app/lib
COPY dist/static /app/static
ENV STATIC_DIR=/app/static HTTP_PORT=8080
USER skuaka
EXPOSE 8080
CMD ["java", "-XX:MaxRAMPercentage=70", "--sun-misc-unsafe-memory-access=allow", "-cp", "/app/lib/*", "skuaka.app.Main"]
