use std::env;
use std::fs;

#[derive(Debug, Clone, Copy, PartialEq)]
enum LabTypes {
    Start,
    Empty,
    Splitter,
    Beam,
}

#[derive(Debug)]
enum Transform {
    MoveDown (usize),
}

impl LabTypes {
    fn from_content(content: &str) -> Vec<Vec<LabTypes>> {
        content.lines().map(|line| {
            line.chars().map(|a| {
                match a {
                    '|' => LabTypes::Beam,
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

    fn take_transformations(transformations: &[Transform], line: &mut [LabTypes], accum: &mut usize) {
        for trans in transformations {
            match trans {
                Transform::MoveDown(x) => {
                    let cell = line[*x];
                    match cell {
                        LabTypes::Empty => {
                            line[*x] = LabTypes::Beam;
                        },
                        LabTypes::Splitter => {
                            line[*x - 1] = LabTypes::Beam;
                            line[*x + 1] = LabTypes::Beam;
                            *accum += 1;
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
                LabTypes::Start | LabTypes::Beam => {
                    Some(Transform::MoveDown(i))
                },
                _ => None,
            })
            .collect()
    }
}

fn simulate(mut grid: Vec<Vec<LabTypes>>) -> usize {
    let mut accum : usize = 0;

    let mut transforms: Vec<Transform> = Transform::from_labtypes_line(&grid[0]);

    for y in 1..grid.len() {
        let line: &mut Vec<LabTypes> = &mut grid[y];
    
        LabTypes::take_transformations(&transforms, line, &mut accum);

        transforms = Transform::from_labtypes_line(&grid[y])
    }

    accum
}


fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path: &String = &args[1];
    let contents: String = fs::read_to_string(file_path).expect("could not read file");
    let grid: Vec<Vec<LabTypes>> = LabTypes::from_content(&contents);
    let result = simulate(grid);
    println!("{}", result)
}