// Render PrimeSieve.swift from PrimeSieve.swift.in and both explicit switches. Every odd factor in
// each range receives a case; this does not test primality or construct masks.
// Run from the PrimeSwift_1bitStriped_u8 package directory:
//   swift Tools/generate-dense.swift --check Sources/PrimeSieveSwift/PrimeSieve.swift
//   swift Tools/generate-dense.swift --write Sources/PrimeSieveSwift/PrimeSieve.swift
// With no arguments, print both marked switches to stdout (legacy interface).
import Foundation

struct DenseSwitch {
    let width: Int
    let firstFactor: Int
    let lastFactor: Int
    let helper: String
    let diagnostic: String

    var beginMarker: String { "        // BEGIN GENERATED DENSE \(width)" }
    var endMarker: String { "        // END GENERATED DENSE \(width)" }
    var label: String { "\(width)-bit \(firstFactor)...\(lastFactor)" }
    var insertion: String { "        // INSERT GENERATED DENSE \(width)\n" }
    var boundToken: String {
        width == 64 ? "{{WORD_DISPATCH_UPPER_BOUND}}" : "{{VECTOR_DISPATCH_UPPER_BOUND}}"
    }

    func generated() -> String {
        var lines = [beginMarker, "        switch p {"]
        for factor in stride(from: firstFactor, through: lastFactor, by: 2) {
            lines.append("        case \(factor):")
            lines.append("            while word + \(factor) <= fullWords {")
            for chunk in 0..<factor {
                // A group begins at an aligned multiple. In chunk j the first
                // multiple has bit offset (-width*j) mod factor; the helper
                // then marks individual multiples by adding factor.
                let first = (factor - (width * chunk) % factor) % factor
                let index = chunk == 0 ? "word" : "word + \(chunk)"
                lines.append("                \(helper)(words, \(index), first: \(first), step: \(factor))")
            }
            lines.append("                word += \(factor)")
            lines.append("            }")
        }
        lines.append("        default:")
        lines.append("            preconditionFailure(\"\(diagnostic) marking requires an odd factor from \(firstFactor) to \(lastFactor)\")")
        lines.append("        }")
        lines.append(endMarker)
        return lines.joined(separator: "\n") + "\n"
    }

    func markedRange(in source: String) throws -> Range<String.Index> {
        let begin = beginMarker + "\n"
        let end = endMarker + "\n"
        guard source.components(separatedBy: begin).count == 2,
              source.components(separatedBy: end).count == 2,
              let start = source.range(of: begin),
              let finish = source.range(of: end, range: start.upperBound..<source.endIndex) else {
            throw NSError(domain: "GenerateDense", code: 2, userInfo: [
                NSLocalizedDescriptionKey: "Missing, duplicated, or reversed \(width)-bit generated-block markers"
            ])
        }
        return start.lowerBound..<finish.upperBound
    }
}

// Runtime dispatch must cover exactly these odd ranges after testing each factor.
// Factor 3 keeps its byte handler; factors above 111 keep the sparse loop.
let switches = [
    DenseSwitch(width: 64, firstFactor: 5, lastFactor: 63,
                helper: "markWord", diagnostic: "Word-dense"),
    DenseSwitch(width: 128, firstFactor: 65, lastFactor: 111,
                helper: "markVectorWord", diagnostic: "Vector-dense"),
]

func fail(_ message: String) -> NSError {
    NSError(domain: "GenerateDense", code: 1, userInfo: [NSLocalizedDescriptionKey: message])
}

func requireOnce(_ text: String, in source: String, label: String) throws {
    guard source.components(separatedBy: text).count == 2 else {
        throw fail("Missing, duplicated, or changed \(label)")
    }
}

func validateSourceMarkers(_ source: String) throws {
    let ranges = try switches.map { try $0.markedRange(in: source) }
    guard ranges[0].upperBound <= ranges[1].lowerBound else {
        throw fail("Generated blocks must be separate, with the 64-bit block first")
    }
}

func render(_ template: String) throws -> String {
    // This is an intentionally narrow source-shape contract, not a Swift parser.
    // A changed dispatch or storage mechanism needs an explicit generator review.
    guard switches.count == 2,
          switches[0].width == 64, switches[0].firstFactor == 5,
          switches[0].helper == "markWord",
          switches[1].width == 128, switches[1].helper == "markVectorWord",
          switches[1].firstFactor == switches[0].lastFactor + 2,
          switches.allSatisfy({ $0.firstFactor & 1 == 1 && $0.lastFactor & 1 == 1 &&
              $0.firstFactor <= $0.lastFactor && $0.lastFactor < $0.width }) else {
        throw fail("Dense metadata must describe consecutive odd ranges after factor 3, within the supported helper widths")
    }
    guard !template.contains("// BEGIN GENERATED DENSE"),
          !template.contains("// END GENERATED DENSE") else {
        throw fail("Template must contain insertion lines, not generated blocks")
    }
    for block in switches {
        try requireOnce(block.insertion, in: template, label: "\(block.width)-bit insertion line")
        try requireOnce(block.boundToken, in: template, label: "\(block.width)-bit dispatch token")
    }
    guard template.range(of: switches[0].insertion)!.upperBound <=
            template.range(of: switches[1].insertion)!.lowerBound else {
        throw fail("Template insertions must put the 64-bit block first")
    }
    let dispatch = """
            while p <= limit / p {
                let candidate = (p - 3) / 2
                if bytes[candidate >> 3] & (UInt8(1) << (candidate & 7)) == 0 {
                    if p < {{WORD_DISPATCH_UPPER_BOUND}} {
                        if p == 3 {
                            markDenseMultiples(of: p)
                        } else {
                            markWordDenseMultiples(of: p)
                        }
                        p += 2
                        continue
                    }

                    if p < {{VECTOR_DISPATCH_UPPER_BOUND}} {
                        markVectorDenseMultiples(of: p)
                        p += 2
                        continue
                    }
    """
    try requireOnce(dispatch, in: template, label: "runtime-tested dense dispatch")
    for block in switches {
        // Scope the width check to the corresponding helper, so swapping two
        // otherwise valid widths cannot accidentally satisfy a global search.
        let signature = "    private func \(block.helper)("
        try requireOnce(signature, in: template, label: "\(block.helper) helper")
        let start = template.range(of: signature)!.upperBound
        guard let end = template.range(of: "\n    }", range: start..<template.endIndex) else {
            throw fail("Missing end of \(block.helper) helper")
        }
        let body = String(template[start..<end.lowerBound])
        try requireOnce("while bit < \(block.width) {", in: body, label: "\(block.helper) bit width")
        let storageType = block.width == 64 ? "UInt64.self" : "SIMD2<UInt64>.self"
        guard body.contains("loadUnaligned(fromByteOffset: offset, as: \(storageType))"),
              body.contains("toByteOffset: offset, as: \(storageType))") else {
            throw fail("Changed \(block.helper) storage width")
        }
    }
    var source = template
    for block in switches {
        source = source.replacingOccurrences(of: block.insertion, with: block.generated())
        source = source.replacingOccurrences(of: block.boundToken, with: String(block.lastFactor + 1))
    }
    guard !source.contains("{{"), !source.contains("}}"),
          !source.contains("// INSERT GENERATED DENSE") else {
        throw fail("Unrecognized template token or insertion")
    }
    try validateSourceMarkers(source)
    return source
}

do {
    let arguments = Array(CommandLine.arguments.dropFirst())
    if arguments.isEmpty {
        for block in switches {
            print(block.generated(), terminator: "")
        }
    } else {
        let usage = "Usage: generate-dense.swift [--check|--write PrimeSieve.swift [--template PrimeSieve.swift.in]]"
        var mode: String?
        var path: String?
        var templatePath: String?
        var index = 0
        while index < arguments.count {
            let flag = arguments[index]
            guard index + 1 < arguments.count, !arguments[index + 1].hasPrefix("--") else { throw fail(usage) }
            switch flag {
            case "--check", "--write":
                guard mode == nil else { throw fail(usage) }
                mode = flag
                path = arguments[index + 1]
            case "--template":
                guard templatePath == nil else { throw fail(usage) }
                templatePath = arguments[index + 1]
            default:
                throw fail(usage)
            }
            index += 2
        }
        guard let mode, let path else { throw fail(usage) }
        let file = URL(fileURLWithPath: path)
        let sourceBytes = try Data(contentsOf: file)
        guard let source = String(data: sourceBytes, encoding: .utf8) else {
            throw fail("PrimeSieve.swift must be UTF-8")
        }
        try validateSourceMarkers(source)
        let templateFile = templatePath.map { URL(fileURLWithPath: $0) } ??
            URL(fileURLWithPath: #filePath).deletingLastPathComponent().appendingPathComponent("PrimeSieve.swift.in")
        let rendered = try render(String(contentsOf: templateFile, encoding: .utf8))
        // Validate everything before one atomic write. The template owns the
        // complete file, including handwritten alignment peels and tails.
        if mode == "--write" {
            try rendered.write(to: file, atomically: true, encoding: .utf8)
            print("Rendered complete PrimeSieve.swift from its template and dense metadata.")
        } else {
            guard sourceBytes == Data(rendered.utf8) else { throw fail("PrimeSieve.swift differs from its complete template rendering") }
            print("Complete source and generated \(switches.map(\.label).joined(separator: " and ")) switches match.")
        }
    }
} catch {
    FileHandle.standardError.write(Data((error.localizedDescription + "\n").utf8))
    exit(1)
}
