package main

import "core:fmt"
import "core:math/rand"
import "core:time"

USE_COLORS :: true;

color :: proc(c: enum{Red, Green, Blue, Clear}, s: string) -> string {
	if (!USE_COLORS || c == .Clear) do return s;

	color_string := "";

	#partial switch c {
	case .Red:   color_string = "\x1b[31m";
	case .Green: color_string = "\x1b[32m";
	case .Blue:  color_string = "\x1b[34m";
	}

	// Also add the clear ANSI code
	return fmt.aprintf("%s%s\x1b[0m", color_string, s);
}

main :: proc() {
	rng := rand.create(u64(time.time_to_unix_nano(time.now())));

	armies := []Army{
		new_army("dragon", "dragons", Army_Statistics {
			c = {
				health = {1000, 2000},
				attack = {20, 50},
			},
			count = {2, 5},
			weapons = []Weapon {
				{ "claws", { 10, 30 } },
				{ "tail", { 30, 50 } },
			},
		}, &rng),
		new_army("human", "humans", Army_Statistics {
			c = {
				health = {60, 80},
				attack = {10, 20},
			},
			count = {100, 500},
			weapons = []Weapon {
				{ "pitchfork", {5, 20} },
				{ "sword", {10, 30} },
			},
		}, &rng),
	};
	defer for army in &armies {
		delete_army(&army);
	}

	for army in armies {
		fmt.printf("enter %d %s\n", len(army.combatents), army.name_plural);
	}

	win := sim_combat_1d(&armies, 0, &rng);
	fmt.printf("\n=> The %s won!\n", win.name_plural);
}

Combatent :: struct($T: typeid) {
	health: T,
	attack: T,
}

Army_Component :: Combatent(int);

Army :: struct {
	name: string,
	name_plural: string,
	combatents: []Army_Component,
	weapons: []Weapon,

	// the index of the currently fighting member
	curr: int,
}

Weapon :: struct {
	name: string,
	attack_power: Min_Max(int),
}

Army_Statistics :: struct {
	using c: Combatent(Min_Max(int)),
	count: Min_Max(int),
	weapons: []Weapon,
}

Min_Max :: struct($T: typeid) {
	min: T,
	max: T,
}

new_army :: proc(
	name: string,
	name_plural: string,
	using stats: Army_Statistics,
	rng: ^rand.Rand,
) -> Army {
	num_combatents := int(rand.float32_range(
		f32(count.min),
		f32(count.max),
		rng,
	));

	combatents := make([]Army_Component, num_combatents);

	for _, i in combatents {
		combatents[i] = {
			health = int(rand.float32_range(
				f32(health.min),
				f32(health.max),
				rng,
			)),

			attack = int(rand.float32_range(
				f32(attack.min),
				f32(attack.max),
				rng,
			)),
		};
	}

	return Army {
		name = name,
		name_plural = name_plural,
		combatents = combatents,
		weapons = weapons,

		// needs to call next
		curr = -1,
	};
}

delete_army :: proc(army: ^Army) {
	delete(army.combatents);
}

curr :: proc(army: ^Army) -> ^Army_Component {
	if army.curr > len(army.combatents) - 1 do return nil;

	return &army.combatents[army.curr];
}

next :: proc(army: ^Army) -> ^Army_Component {
	army.curr += 1;
	return curr(army);
}

sim_combat_1d :: proc(
	armies: ^[]Army,
	starting_turn: int,
	rng: ^rand.Rand,
) -> ^Army {
	{
		l := len(armies);
		assert(l == 2, fmt.tprintf("Expected two armies, but got %d", l));
	}

	num_armies := len(armies);
	curr_turn := starting_turn;

	combatents := make([]^Army_Component, num_armies);
	defer delete(combatents);

	for army, i in armies {
		combatents[i] = next(&army);
	}

	for {
		curr_turn = combat_turn(armies, curr_turn, rng);

		if winner := has_winner(armies); winner != nil {
			return winner;
		}
	}
}

has_winner :: proc(armies: ^[]Army) -> ^Army {
	alive := 0;
	winner: ^Army = nil;

	for army, i in armies {
		if curr(&army) == nil do continue;
		alive += 1;

		if alive > 1 do return nil;
		winner = &army;
	}

	return winner;
}

combat_turn :: proc(
	armies: ^[]Army,
	turn: int,
	rng: ^rand.Rand,
) -> (next_turn: int) {
	next_turn = switch_turn(len(armies), turn);

	curr_combatent := curr(&armies[turn]);
	if curr_combatent == nil do return;

	for army, i in armies {
		// don't attack yourself
		if i == turn do continue;

		oponent := curr(&army);
		if oponent == nil do continue;

		// attack reports if a kill happened
		if attack(
			curr_combatent,
			oponent,
			armies[turn].name,
			armies[next_turn].name,
			rng,
		) {
			next(&army); // may nil-ify this army
		}
	}

	return;
}

attack :: proc(
	attacker, blocker: ^Army_Component,
	attacker_name, blocker_name: string,
	rng: ^rand.Rand,
) -> bool {
	damage := int(rand.float32_range(
		f32(0),
		f32(attacker.attack),
		rng,
	));
	blocker.health -= damage;

	kill := blocker.health <= 0;

	attacker_str := color(.Red, attacker_name);
	defer delete(attacker_str);
	blocker_str := color(.Blue, blocker_name);
	defer delete(blocker_str);
	kill_str := kill ? color(.Red, " It's a killing blow!") : "";
	defer if kill_str != "" do delete(kill_str);

	fmt.printf(
		"The %s attacks the %s for %d damage!%s\n",
		attacker_str,
		blocker_str,
		damage,
		kill_str,
	);

	return kill;
}

switch_turn :: proc(num_armies: int, curr: int) -> int {
	return (curr + 1) % num_armies;
}

