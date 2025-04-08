FROM golang:1.24-bookworm AS build

WORKDIR /src
COPY ./go.mod ./go.sum ./
COPY ./cmd/server/main.go /main.go

RUN CGO_ENABLED=0 go build -o /server /main.go

FROM gcr.io/distroless/static-debian12

COPY --from=build /server /

EXPOSE 8080

CMD ["/server"]
