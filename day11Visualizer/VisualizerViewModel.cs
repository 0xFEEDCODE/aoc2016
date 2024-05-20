using System.ComponentModel;
using System.Runtime.CompilerServices;
using System.Windows;
using System.Windows.Controls;
using System.Windows.Media;
using System.Windows.Media.Animation;
using System.Windows.Shapes;

namespace day11Visualizer;

public enum State
{
    PreSolve,
    Solve,
    Visualize
}

public class StateMachine
{
    public State CurrentState { get; private set; } = State.PreSolve;

    public void ExecuteTransition()
    {
        CurrentState = CurrentState switch
        {
            State.PreSolve => State.Solve,
            State.Solve => State.Visualize,
            State.Visualize => throw new Exception()
        };
    }
}

public class VisualizerViewModel : INotifyPropertyChanged
{
    public event PropertyChangedEventHandler? PropertyChanged;
    
    public bool IsStatePreSolve => stateMachine.CurrentState is State.PreSolve;
    public bool IsStateSolve => stateMachine.CurrentState is State.Solve;
    public bool IsStateVisualize => stateMachine.CurrentState is State.Visualize;

    private string puzzleInput;
    public string PuzzleInput
    {
        get => puzzleInput;
        set
        {
            SetField(ref puzzleInput, value);
            SolveCommand.RaiseCanExecuteChanged();
        }
    }
    
    private string solveProgress;
    public string SolveProgress
    {
        get => solveProgress;
        set => SetField(ref solveProgress, value);
    }

    public DelegateCommand SolveCommand { get; }
    public DelegateCommand AnimateCommand { get; }

    private StateMachine stateMachine;

    private Canvas canvas;
    private Slider animationSlider;
    private System.Timers.Timer animationTimer;
    private string[][] result;

    private int _animationSliderValue;
    private int animationSliderValue
    {
        get => _animationSliderValue;
        set
        {
            _animationSliderValue = value;
            OnPropertyChanged(nameof(Step));
        }
    }

    public int Step => animationSliderValue;

    public VisualizerViewModel(ref Canvas canvas, ref Slider slider)
    {
        this.canvas = canvas;
        animationSlider = slider;
        
        stateMachine = new StateMachine();
        puzzleInput = string.Empty;

        var i = 0;
        
        SolveCommand = new DelegateCommand(_ =>
        {
            ExecuteTransition();

            Task.Factory.StartNew(() =>
            {
                using var solve = aoc_2016.day11Visualizer.solve(PuzzleInput.Split("\n")).GetEnumerator();
                while (solve.MoveNext())
                {
                    SolveProgress += $"Reached step - {solve.Current}\n";
                }

                ExecuteTransition();
            }).ContinueWith(_ =>
            {
                result = aoc_2016.day11Visualizer.searchResult.Select(r => r.Split('\n').ToArray()).ToArray();

                animationTimer =  new System.Timers.Timer();
                this.canvas.Dispatcher.Invoke(() =>
                {
                    InitializeVisuals(ref this.canvas, result[0]);
                    UpdateVisuals(result[0]);
                });
                
                animationTimer.Elapsed += (_, _) =>
                {
                    this.canvas.Dispatcher.Invoke(() =>
                    {
                        if (animationSlider.Value + 1 > animationSlider.Maximum)
                        {
                            animationTimer.Stop();
                        }
                        else
                        {
                            animationSliderValue = (int)animationSlider.Value + 1;
                            animationSlider.Dispatcher.Invoke(() =>
                            {
                                animationSlider.Value = animationSliderValue;
                            });
                            this.canvas.Dispatcher.Invoke(() => UpdateVisuals(result[animationSliderValue]));
                        }
                    });
                };
                
                animationSlider.Dispatcher.Invoke(() =>
                {
                    animationSlider.Maximum = result.Length-1;
                    animationSlider.Delay = 1000;
                    animationSlider.ValueChanged += (_, _) =>
                    {

                        if (animationSliderValue == (int)animationSlider.Value)
                        {
                            return;
                        }

                        animationTimer.Stop();
                        animationSliderValue = (int)animationSlider.Value;
                        
                        if (animationSliderValue < result.Length)
                        {
                            this.canvas.Dispatcher.Invoke(() =>
                            {
                                UpdateVisuals(result[animationSliderValue]);
                            });
                        }
                    };
                    animationSlider.Value = 0;
                });
                
                animationTimer.Interval = 1000;
            });

        }, _ => stateMachine.CurrentState is State.PreSolve && puzzleInput.Length > 0);

        AnimateCommand = new DelegateCommand((_ =>
        {
            animationSliderValue = 0;
            animationSlider.Value = 0;
            animationTimer.Stop();
            
            this.canvas.Dispatcher.Invoke(() =>
            {
                UpdateVisuals(result[animationSliderValue]);
            });
            
            animationTimer.Start();
        }));
    }

    private void ExecuteTransition()
    {
        stateMachine.ExecuteTransition();
        OnPropertyChanged(nameof(IsStatePreSolve));
        OnPropertyChanged(nameof(IsStateSolve));
        OnPropertyChanged(nameof(IsStateVisualize));
    }

    protected virtual void OnPropertyChanged([CallerMemberName] string? propertyName = null)
    {
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    }

    protected bool SetField<T>(ref T field, T value, [CallerMemberName] string? propertyName = null)
    {
        if (EqualityComparer<T>.Default.Equals(field, value)) return false;
        field = value;
        OnPropertyChanged(propertyName);
        return true;
    }

    private static Dictionary<string, SolidColorBrush> brushes = new();
    private static Dictionary<string, int> indexes = new();
    private static Dictionary<string, Rectangle> chipsObjects = new();
    private static Dictionary<string, Ellipse> generatorsObjects = new();
    private static Line[] lines = [];

    private static SolidColorBrush[] vibrantBrushes =
    [
        new(Color.FromRgb(255, 105, 97)),  // Vibrant red
        new(Color.FromRgb(255, 179, 71)),  // Vibrant orange
        new(Color.FromRgb(255, 233, 84)),  // Vibrant yellow
        new(Color.FromRgb(119, 221, 119)), // Vibrant green
        new(Color.FromRgb(77, 182, 172)),  // Vibrant teal
        new(Color.FromRgb(77, 113, 214)),  // Vibrant blue
        new(Color.FromRgb(142, 68, 173)),   // Vibrant purple
        new(Color.FromRgb(255, 192, 203)), // New color: Pink
        new(Color.FromRgb(75, 0, 130))     // New color: Indigo
    ];
    
    private static void InitializeVisuals(ref Canvas canvas, string[] data)
    {
        const int height = 450;
        const int width = 800;
        
        const int oneWidth = height / 100;
        const int oneHeight = height / 100;

        const int lineXStart = oneWidth * 10;
        const int lineXEnd = width - (oneWidth * 10);
        
        const int spacing = 20;
        
        canvas.Children.Clear();

        var brush = new SolidColorBrush(Colors.White);
        var temp = new List<Line>();
        for (var i = 1; i <= 4; i++)
        {
            var line = NewLine(lineXStart, lineXEnd, oneHeight * 20 * i, oneHeight * 20 * i, brush);
            temp.Add(line);
            canvas.Children.Add(line);
        }
        lines = temp.ToArray();

        foreach (var i in Enumerable.Range(0, data.Length))
        {
            var chips = data[i].Split("||")[0].Split(' ')[1].Split(';').SkipLast(1).ToArray();
            var generators = data[i].Split("||")[1].Split(' ')[2].Split(';').SkipLast(1).ToArray();
            
            foreach(var j in Enumerable.Range(0, chips.Length))
            {
                var chip = chips[j];

                indexes.TryAdd(chip, indexes.Count+1);
                var idx = indexes[chip];
                brushes.TryAdd(chip, vibrantBrushes[idx]);

                var rect = NewRectangle();
                rect.RadiusX = 5;
                rect.RadiusY = 5;
                
                rect.Fill = brushes[chip];
                canvas.Children.Add(rect);
                Canvas.SetTop(rect, temp[i].Y1 - (rect.Height/2));
                Canvas.SetLeft(rect, temp[i].X1 + (oneWidth * spacing * idx));
                chipsObjects.Add(chip, rect);
            }
            
            foreach(var j in Enumerable.Range(0, generators.Length))
            {
                var gen = generators[j];
                
                indexes.TryAdd(gen, indexes.Count+1);
                var idx = indexes[gen];
                brushes.TryAdd(gen, vibrantBrushes[idx]);
                
                var ellipse = NewEllipse();
                
                ellipse.Fill = brushes[gen];
                
                ellipse.StrokeThickness = 2;
                ellipse.Stroke = new SolidColorBrush() { Color = Color.FromRgb(55,55,55)};
                
                canvas.Children.Add(ellipse);
                Canvas.SetTop(ellipse, temp[i].Y1 - (ellipse.Height / 2));
                Canvas.SetLeft(ellipse, temp[i].X1 + (oneWidth * spacing * idx) + 2.5);
                generatorsObjects.Add(gen, ellipse);
            }
        }
    }
    
    private static void UpdateVisuals(string[] data)
    {
        const int height = 450;
        
        const int oneWidth = height / 100;
        
        const int spacing = 20;
        
        foreach (var i in Enumerable.Range(0, data.Length))
        {
            var chips = data[i].Split("||")[0].Split(' ')[1].Split(';').SkipLast(1).ToArray();
            var generators = data[i].Split("||")[1].Split(' ')[2].Split(';').SkipLast(1).ToArray();
            
            foreach(var j in Enumerable.Range(0, chips.Length))
            {
                var chip = chips[j];
                var idx = indexes[chip];
                var rect = chipsObjects[chip];
                
                var animation = new DoubleAnimation
                {
                    To = lines[i].Y1 - (rect.Height/2),
                    Duration = TimeSpan.FromMilliseconds(500),
                    EasingFunction = new QuadraticEase()
                };
                Timeline.SetDesiredFrameRate(animation, 120);
                rect.BeginAnimation(Canvas.TopProperty, animation);
                
                Canvas.SetTop(rect, lines[i].Y1 - (rect.Height/2));
                Canvas.SetLeft(rect, lines[i].X1 + (oneWidth * spacing * idx));
            }
            
            foreach(var j in Enumerable.Range(0, generators.Length))
            {
                var gen = generators[j];
                var idx = indexes[gen];
                var ellipse = generatorsObjects[gen];
                
                var animation = new DoubleAnimation
                {
                    To = lines[i].Y1 - (ellipse.Height / 2),
                    Duration = TimeSpan.FromMilliseconds(500),
                    EasingFunction = new QuadraticEase()
                };
                Timeline.SetDesiredFrameRate(animation, 120);
                ellipse.BeginAnimation(Canvas.TopProperty, animation);
                
                Canvas.SetTop(ellipse, lines[i].Y1 - (ellipse.Height / 2));
                Canvas.SetLeft(ellipse, lines[i].X1 + (oneWidth * spacing * idx) + 2.5);
            }
        }
    }

    private static Ellipse NewEllipse() => new() { Width = 35, Height = 35 };
    private static Rectangle NewRectangle() => new() { Width = 40, Height = 40 };

    private static Line NewLine(double x1, double x2, double y1, double y2, Brush brush)
    {
        var line = new Line
        {
            X1 = x1,
            X2 = x2,
            Y1 = y1,
            Y2 = y2,
            StrokeThickness = 1.5,
            Stroke = brush,
            SnapsToDevicePixels = true
        };

        line.SetValue(RenderOptions.EdgeModeProperty, EdgeMode.Aliased);

        return line;
    }
    
    private void AnimateSlider(double newValue)
    {
        var animation = new DoubleAnimation(newValue, new Duration(TimeSpan.FromMilliseconds(100)));
        animationSlider.BeginAnimation(Slider.ValueProperty, animation);
    }
}