import Foundation

struct Alternative {
    var name: String
    init(_ string: String) {
        self.name = string
    }
}

extension Alternative: Equatable {}
extension Alternative: Comparable {
    static func < (lhs: Alternative, rhs: Alternative) -> Bool {
        lhs.name < rhs.name
    }
}
extension Alternative: Hashable {
    public func hash(into hasher: inout Hasher) {
        hasher.combine(name)
    }
}

extension Alternative: CustomDebugStringConvertible {
    var debugDescription: String {
        name
    }
}

typealias LinearPreference = Array<Alternative>

extension Array where Element == Alternative {
    var preferenceDescription: String {
        self.map { $0.name }
            .joined(separator: preferenceSymbol.description)
    }
}
struct PreferenceProfile {
    var linearPreference: LinearPreference
    var count: Int
}

extension PreferenceProfile: Hashable {
    static func == (lhs: PreferenceProfile, rhs: PreferenceProfile) -> Bool {
        lhs.linearPreference == rhs.linearPreference && lhs.count == rhs.count
    }

    public func hash(into hasher: inout Hasher) {
        hasher.combine(count)
        hasher.combine(linearPreference)
    }
}

typealias PreferenceSet = Set<PreferenceProfile>


let a = Alternative("a")
let b = Alternative("b")
let c = Alternative("c")
let d = Alternative("d")
let e = Alternative("e")


let preferenceSymbol = Character("≻")
let rawChoices = [
    "b≻d≻a≻e≻c",
    "b≻d≻a≻e≻c",
    "b≻d≻a≻e≻c",

    "d≻a≻c≻e≻b",
    "d≻a≻c≻e≻b",
    "d≻a≻c≻e≻b",

    "b≻c≻e≻d≻a",
    "b≻c≻e≻d≻a",
    "b≻c≻e≻d≻a",
    "b≻c≻e≻d≻a",

    "a≻e≻c≻d≻b",
    "a≻e≻c≻d≻b",
    "a≻e≻c≻d≻b",
    "a≻e≻c≻d≻b",

    "a≻b≻c≻e≻d",
    "e≻c≻d≻a≻b"
]

extension LinearPreference {
    init?(_ string: String) {
        self = string.split { preferenceSymbol == $0 }
            .compactMap { Alternative(String($0)) }
    }
}

let linearPreferences = rawChoices.compactMap { LinearPreference($0) }


extension PreferenceSet {
    /**
     Iterates over a collection of `LinearPreference` and groups them into `PreferenceProfiles`
     - Note: complexity: O(n^2)
     */
    init(_ preferences: [LinearPreference]) {
        var set = PreferenceSet()

        // this iterates over the linear preferences if the loop is O(1) then
        // it would be O(n).  Since the loop is also O(n) this ends up being
        // n square
        for p in preferences {

            // this iterates over the preference until p is found O(n)
            let count = preferences.filter { $0 == p }.count

            set.insert(
                PreferenceProfile(
                    linearPreference: p,
                    count: count
                )
            )
        }
        self = set
    }
}

let preferenceSet = PreferenceSet(linearPreferences)


extension LinearPreference {
    /// Check whether in this `LinearPreference` `Alternative` x is preferred over `Alternative` y
    /// - note: This function is O(2n)
    func prefers(_ x: Alternative, over y: Alternative) -> Bool {
        guard let xIndex = self.firstIndex(of: x),
              let yIndex = self.firstIndex(of: y) else { return false }
        return xIndex < yIndex
    }
}


/// Consider a set A = {1,...,m} of alternatives and a set N = {1,...,n} of voters with preferences ≻i ∈ L(A) for all i ∈ N . Here, we denote by L(X) the set of all linear orders on a finite set X, that is, the set of all binary relations on X that are complete, transitive, and asymmetric. For a given preference profile R = (≻1 , . . . , ≻n ) ∈ L(A)n , the majority margin mR(x,y) of x over y is defined as the difference between the number of voters who prefer x to y and the number of voters who prefer y to x, that is,
/// mR(x,y)=|{i∈N:x≻i y}|−|{i∈N:y≻i x}|.
/// - Parameter x: x an `Alternative`
/// - Parameter y: y an `Alternative`
/// - Parameter preferenceSet: a preference set `PreferenceSet`
/// - Returns the majority margin
/// - Note: this function is`O(n * 2n)`
func majorityMargin(_ x: Alternative,_ y: Alternative, preferenceSet: PreferenceSet) -> Int {
    var xOverY: Int = 0
    var yOverX: Int = 0

    for profile in preferenceSet {
        xOverY += (profile.linearPreference.prefers(x, over: y) ? 1 : 0) * profile.count
        yOverX += (profile.linearPreference.prefers(y, over: x) ? 1 : 0) * profile.count
    }

    return abs(xOverY) - abs(yOverX)
}


/// How many time `Alternative` x dominates over `Alternative` y on the `PreferenceSet` preferenceSet
/// - Note: this function is`O(n * n)`
func alternative(_ x: Alternative, dominates y: Alternative, on preferenceSet: PreferenceSet) -> Int {
    var xOverY = 0
    for profile in preferenceSet {
        xOverY += (profile.linearPreference.prefers(x, over: y) ? 1 : 0) * profile.count
    }

    return xOverY
}

/**

a ==> b 2
a ==> c 6
a ==> e 6
e ==> d 4
d ==> a 6
c ==> d 4
 */
majorityMargin(a, b, preferenceSet: preferenceSet)
majorityMargin(a, c, preferenceSet: preferenceSet)
majorityMargin(a, e, preferenceSet: preferenceSet)
majorityMargin(e, d, preferenceSet: preferenceSet)
majorityMargin(d, a, preferenceSet: preferenceSet)
majorityMargin(c, d, preferenceSet: preferenceSet)


struct Tournament {
    struct Arc: Hashable {
        var x: Alternative
        var y: Alternative
        var m: Int

    }
    var alternatives: [Alternative]
    var arcs: [Arc]
    var preferenceSet: PreferenceSet

    init(alternatives: [Alternative], preferenceSet: PreferenceSet) {
        let entries = Array(alternatives)
        self.preferenceSet = preferenceSet
        self.alternatives = alternatives
        var arcs = [Arc]()

        for i in (0 ..< entries.count - 1) {
            for j in (i ..< entries.count) {
                guard i != j else { continue }
                let m = majorityMargin(entries[i], entries[j], preferenceSet: preferenceSet)
                guard m != 0 else { continue }
                arcs.append(
                    Arc(
                        x: entries[i],
                        y: entries[j],
                        m: m
                    )
                )
            }
        }

        self.arcs = arcs
    }

    var description: String {
        var desc = """
                      Tournament:
                      Alternatives: \(self.alternatives)
                   """

        for arc in arcs {
            if arc.m >= 0 {
                desc += "\n \(arc.y) ==> \(arc.x) [\(arc.m)]"
            } else {
                desc += "\n \(arc.x) ==> \(arc.y) [\(abs(arc.m))]"
            }
        }

        return desc
    }
}


/**

a ==> b 2
a ==> c 6
a ==> e 6
e ==> d 4
d ==> a 6
c ==> d 4
 */
let tournamentA = Tournament(
    alternatives: [a, b, c, d, e],
    preferenceSet: preferenceSet
)


tournamentA.description


extension Tournament {
    func graphAsMatrix() -> [[Int]] {
        var m = [[Int]]()
        self.alternatives.forEach { _ in m.append([Int](repeating: 0, count: alternatives.count))}

        var lookup: [Alternative : Int] = [:]

        self.alternatives.enumerated().forEach { e in lookup[e.element] = e.offset }

        for arc in arcs {
            let xIndex = lookup[arc.x]!
            let yIndex = lookup[arc.y]!
            m[xIndex][yIndex] = abs(arc.m)
        }
        return m
    }

    /// Creates a matrix with every preference in the set.
    /// Theorem 4.1 (Debord, 1987). Let (A, M ) be a weighted tournament.
    /// Then M = MR for some profile R of preferences over A if and only if all off-diagonal elements of M have the same parity.
    /// - Note: This function is O(n
    func preferenceMatrix() -> [[Int]] {
        var m = [[Int]]()
        self.alternatives.forEach { _ in m.append([Int](repeating: 0, count: alternatives.count))}

        // calculate cartessian product and exclude reflective arcs
        for i in (0..<self.alternatives.count) {
            for j in (0..<self.alternatives.count) {
                guard i != j else { continue }

                let x = self.alternatives[i]
                let y = self.alternatives[j]

                m[i][j] = abs(majorityMargin(x, y, preferenceSet: preferenceSet))
            }
        }

        return m
    }

    /// For a tournament (A, ≻) and an alternative a ∈ A, we denote by Dominion(a) the dominion of a, that is,
    /// Dominion(a) = { b ∈ A : a ≻ b }
    func dominion(of a: Alternative, over b: Alternative) -> [LinearPreference] {
       let preferences = self.preferenceSet.map { $0.linearPreference }

        return preferences.filter { $0.prefers(a, over: b) }
    }

    /// For a tournament (A, ≻) and an alternative a ∈ A, we denote by Dominators(a) the dominators of a, that is,
    /// Dominators(a) = { b ∈ A : b ≻ a }.
    func dominators(of a: Alternative, b: Alternative) -> [LinearPreference] {
        self.preferenceSet.map { $0.linearPreference }
            .filter { $0.prefers(b, over: a) }
    }
}

tournamentA.graphAsMatrix()


/**

 tournamentA.graphAsMatrix()
 [
 [0, 2, 6, 6, 6],
 [0, 0, 0, 0, 0],
 [0, 0, 0, 4, 0],
 [0, 0, 0, 0, 4],
 [0, 0, 0, 0, 0]
 ]
 */

tournamentA.preferenceMatrix()

/**
 tournamentA.preferenceMatrix() prints
 [
 [0, 2, 6, 6, 6],
 [2, 0, 0, 0, 0],
 [6, 0, 0, 4, 0],
 [6, 0, 4, 0, 4],
 [6, 0, 0, 4, 0]
 ]
 */


tournamentA.dominion(of: Alternative("a"), over: Alternative("b")).forEach{ p in print(p.preferenceDescription)}
/**

 tournamentA.dominion(of: Alternative("a"), over: Alternative("b")).forEach{ p in print(p.preferenceDescription)} prints
 
 a≻e≻c≻d≻b
 e≻c≻d≻a≻b
 a≻b≻c≻e≻d
 d≻a≻c≻e≻b

 */
