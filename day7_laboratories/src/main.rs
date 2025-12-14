use std::env;
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq)]
enum LabTypes {
    Start,
    Empty,
    Splitter,
    Beam(usize),
}

#[derive(Debug)]
enum Transform {
    MoveDown (usize, usize),
}

impl LabTypes {
    fn from_content(content: &str) -> Vec<Vec<LabTypes>> {
        content.lines().map(|line| {
            line.chars().map(|a| {
                match a {
                    '^' => LabTypes::Splitter,
                    '.' => LabTypes::Empty,
                    'S' => LabTypes::Start,
                    _ => LabTypes::Empty,
                }
            })
            .collect()
        })
        .collect()
    }

    fn split_handle(cell: &mut LabTypes, x: usize) {
        match *cell {
            LabTypes::Beam(y) => {
                *cell = LabTypes::Beam(y + x);
            }
            LabTypes::Empty => {
                *cell = LabTypes::Beam(x);
            }
            _ => {}
        }
    }

    fn take_transformations(transformations: &[Transform], line: &mut [LabTypes]) {
        for trans in transformations {
            match trans {
                Transform::MoveDown(i, x) => {
                    let cell = line[*i];
                    match cell {
                        LabTypes::Empty => {
                            line[*i] = LabTypes::Beam(*x);
                        },
                        LabTypes::Beam(y) => {
                            line[*i] = LabTypes::Beam(y + *x);
                        }
                        LabTypes::Splitter => {
                            LabTypes::split_handle(&mut line[*i - 1], *x);
                            LabTypes::split_handle(&mut line[*i + 1], *x);
                        },
                        _ => {},
                    }
                },
            } 
        }
    }
}

impl Transform {
    fn from_labtypes_line(line: &[LabTypes]) -> Vec<Transform> {
        line.iter()
            .enumerate()
            .filter_map(|(i, cell)| match cell {
                LabTypes::Start => {
                    Some(Transform::MoveDown(i, 1))
                },
                LabTypes::Beam(x) => {
                    Some(Transform::MoveDown(i, *x))
                },
                _ => None,
            })
            .collect()
    }
}

fn simulate(mut grid: Vec<Vec<LabTypes>>) -> usize {
    let mut transforms: Vec<Transform> = Transform::from_labtypes_line(&grid[0]);

    for y in 1..grid.len() {
        let line: &mut Vec<LabTypes> = &mut grid[y];
    
        LabTypes::take_transformations(&transforms, line);

        transforms = Transform::from_labtypes_line(&grid[y])
    }

    let last = &grid[grid.len() - 1];
    last.iter().map(|cell| {
        match cell {
            LabTypes::Beam(x) => *x,
            _ => 0,
        }
    }).sum()
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];
    let contents: String = fs::read_to_string(file_path).expect("could not read file");
    let grid: Vec<Vec<LabTypes>> = LabTypes::from_content(&contents);
    let result = simulate(grid);
    println!("{}", result)
}