// swift-tools-version:5.7
// The swift-tools-version declares the minimum version of Swift required to build this package.

import PackageDescription

let package = Package(
    name: "PrimeSieveSwift",
    platforms: [.macOS(.v10_15)],
    products: [
        .executable(
            name: "PrimeSieveSwift",
            targets: ["PrimeSieveSwift"]),
    ],
    dependencies: [
        .package(url: "https://github.com/apple/swift-argument-parser", from: "0.0.1")
    ],
    targets: [
        // The observer stays opaque to the executable's optimizer. Do not enable
        // cross-module optimization when building this package.
        .target(
            name: "BenchmarkObserver",
            swiftSettings: [
                .unsafeFlags(["-disable-cmo"], .when(configuration: .release)),
            ]),
        .executableTarget(
            name: "PrimeSieveSwift",
            dependencies: [
                "BenchmarkObserver",
                .product(name: "ArgumentParser", package: "swift-argument-parser"),
            ],
            swiftSettings: [
                .unsafeFlags(["-whole-module-optimization", "-disable-cmo"], .when(configuration: .release)),
            ]),
    ]
)
