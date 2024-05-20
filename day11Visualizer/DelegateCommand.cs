using System.Windows.Input;

namespace day11Visualizer;

public class DelegateCommand(
    Action<object> execute,
    Predicate<object>? canExecute) : ICommand
{
    public event EventHandler? CanExecuteChanged;

    public DelegateCommand(Action<object> execute)
        : this(execute, null)
    {
    }

    public bool CanExecute(object? parameter)
    {
        return (canExecute == null || canExecute(parameter));
    }

    public void Execute(object? parameter)
    {
        execute(parameter);
    }

    public void RaiseCanExecuteChanged()
    {
        CanExecuteChanged?.Invoke(this, EventArgs.Empty);
    }
}