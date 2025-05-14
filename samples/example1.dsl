ingredients {
    eggs            { protein: 7, fat: 5, carbs: 0, potassium: 100, magnesium: 10, sodium: 40 }
    bacon           { protein: 3, fat: 10, carbs: 0, potassium: 50, magnesium: 5, sodium: 100 }
    salmon          { protein: 20, fat: 10, carbs: 0, potassium: 250, magnesium: 25, sodium: 50 }
    avocado         { protein: 1, fat: 8, carbs: 1, potassium: 150, magnesium: 20, sodium: 5 }
    chickenThigh   { protein: 30, fat: 15, carbs: 0, potassium: 350, magnesium: 20, sodium: 40 }
    chickenBreast  { protein: 40, fat: 5, carbs: 0, potassium: 400, magnesium: 25, sodium: 30 }
    spinach         { protein: 2, fat: 0, carbs: 1, potassium: 200, magnesium: 20, sodium: 5 }
}

mealplan {
    { day Monday {
        meal breakfast {
            eggs 2
            bacon 3
        }
        meal lunch {
            salmon 200
            avocado 100
        }
        meal dinner {
            chickenThigh 150
            spinach 50
        }
    }}
    
    { day Tuesday {
        meal breakfast {
            eggs 3
            bacon 2
            avocado 50
        }
        meal lunch {
            chickenBreast 200
            spinach 100
        }
        meal dinner {
            salmon 150
            avocado 100
        }
    }}
    
    { day Wednesday {
        meal breakfast {
            eggs 2
            bacon 4
        }
        meal lunch {
            salmon 250
            spinach 75
        }
        meal dinner {
            chickenThigh 200
            avocado 150
        }
    }}
} 

