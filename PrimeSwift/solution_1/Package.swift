// swift-tools-version:5.3
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
        .target(
            name: "PrimeSieveSwift",
            dependencies: [.product(name: "ArgumentParser", package: "swift-argument-parser"),]),
    ]
)
